#!/usr/bin/env bash
set -euo pipefail

JFROG_HOSTNAME="databricks.jfrog.io"
JFROG_REALM="Artifactory Realm"
JFROG_USERNAME="gha-service-account"
JFROG_URL="https://${JFROG_HOSTNAME}/artifactory/db-maven/"
MAVEN_CENTRAL_URL="https://maven-central.storage.googleapis.com/maven2/"

replace_repo1_in_mill() {
  python3 - "$1" << 'PY'
from pathlib import Path
import sys

path = Path("mill")
path.write_text(path.read_text().replace("https://repo1.maven.org/maven2", sys.argv[1]))
PY
}

force_no_server_in_mill() {
  python3 - << 'PY'
from pathlib import Path

path = Path("mill")
text = path.read_text()
old = '"${MILL}" $MILL_FIRST_ARG -D "mill.main.cli=${MILL_MAIN_CLI}" "$@"'
new = '"${MILL}" --no-server $MILL_FIRST_ARG -D "mill.main.cli=${MILL_MAIN_CLI}" "$@"'
path.write_text(text.replace(old, new))
PY
}

add_curl_retries_in_mill() {
  python3 - << 'PY'
from pathlib import Path

path = Path("mill")
text = path.read_text()
old = 'curl -f -L -o "${MILL_TEMP_DOWNLOAD_FILE}" "${MILL_DOWNLOAD_URL}"'
new = 'curl --retry 5 --retry-delay 2 --retry-all-errors -f -L -o "${MILL_TEMP_DOWNLOAD_FILE}" "${MILL_DOWNLOAD_URL}"'
path.write_text(text.replace(old, new))
PY
}

mkdir -p ~/.sbt
if [[ "${JFROG_ACCESS_TOKEN}" == "dummy" ]]; then
  # Fork PRs cannot mint the Databricks JFrog OIDC token. Use public Maven Central directly so new
  # public dependency versions that have not yet been mirrored to JFrog can still be validated.
  cat > ~/.sbt/repositories << EOF
[repositories]
  local
  maven-central: ${MAVEN_CENTRAL_URL}
EOF
  echo "COURSIER_REPOSITORIES=${MAVEN_CENTRAL_URL}" >> "$GITHUB_ENV"
  mill_version="$(sed -n -E 's#^//\|[[:space:]]*mill-version:[[:space:]]*([^[:space:]]+).*#\1#p' build.mill | head -n 1)"
  if [[ -z "${mill_version}" ]]; then mill_version="1.1.0"; fi
  echo "MILL_VERSION=${mill_version%-jvm}-jvm" >> "$GITHUB_ENV"
  replace_repo1_in_mill "${MAVEN_CENTRAL_URL%/}"
  force_no_server_in_mill
  add_curl_retries_in_mill
  {
    echo "-Djava.net.preferIPv4Stack=true"
    echo "-Djdk.tls.client.protocols=TLSv1.2"
  } >> .mill-jvm-opts
else
  # Configure sbt repositories
  cat > ~/.sbt/repositories << EOF
[repositories]
  local
  databricks-jfrog: ${JFROG_URL}
EOF

  # Configure sbt credentials
  cat > ~/.sbt/.credentials << EOF
realm=${JFROG_REALM}
host=${JFROG_HOSTNAME}
user=${JFROG_USERNAME}
password=${JFROG_ACCESS_TOKEN}
EOF

  mkdir -p ~/.config/coursier
  {
    echo "jfrog.host=${JFROG_HOSTNAME}"
    echo "jfrog.realm=${JFROG_REALM}"
    echo "jfrog.username=${JFROG_USERNAME}"
    echo "jfrog.password=${JFROG_ACCESS_TOKEN}"
  } > ~/.config/coursier/credentials.properties

  echo "COURSIER_REPOSITORIES=${JFROG_URL}" >> "$GITHUB_ENV"
  replace_repo1_in_mill "https://${JFROG_USERNAME}:${JFROG_ACCESS_TOKEN}@${JFROG_URL:8}"
fi

# Configure global.sbt to load credentials
mkdir -p ~/.sbt/1.0
cat > ~/.sbt/1.0/global.sbt << 'EOF'
def sbtCredentialsFile = file(sys.props("user.home")) / ".sbt" / ".credentials"
credentials ++= {
  if (sbtCredentialsFile.exists()) List(Credentials(sbtCredentialsFile))
  else Nil
}
EOF
