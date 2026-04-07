#!/usr/bin/env bash
set -euo pipefail

JFROG_HOSTNAME="databricks.jfrog.io"
JFROG_REALM="Artifactory Realm"
JFROG_USERNAME="gha-service-account"
JFROG_URL="https://${JFROG_HOSTNAME}/artifactory/db-maven/"

# Configure sbt repositories
mkdir -p ~/.sbt
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

# Configure global.sbt to load credentials
mkdir -p ~/.sbt/1.0
cat > ~/.sbt/1.0/global.sbt << 'EOF'
def sbtCredentialsFile = file(sys.props("user.home")) / ".sbt" / ".credentials"
credentials ++= {
  if (sbtCredentialsFile.exists()) List(Credentials(sbtCredentialsFile))
  else Nil
}
EOF


mkdir -p ~/.config/coursier
{
  echo "jfrog.host=${JFROG_HOSTNAME}"
  echo "jfrog.realm=${JFROG_REALM}"
  echo "jfrog.username=${JFROG_USERNAME}"
  echo "jfrog.password=${JFROG_ACCESS_TOKEN}"
} > ~/.config/coursier/credentials.properties

echo "COURSIER_REPOSITORIES=${JFROG_URL}" >> "$GITHUB_ENV"
sed -i "s|https://repo1.maven.org/maven2|https://${JFROG_USERNAME}:${JFROG_ACCESS_TOKEN}@${JFROG_URL:8}|g" ./mill
