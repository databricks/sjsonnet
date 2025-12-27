#!/bin/bash
set -e

# Install SDKMAN if not present
if [ ! -d "$HOME/.sdkman" ]; then
    echo "Installing SDKMAN..."
    curl -s "https://get.sdkman.io" | bash
else
    echo "SDKMAN already installed."
fi

# Source SDKMAN
source "$HOME/.sdkman/bin/sdkman-init.sh"

echo "Installing GraalVM Java 25.0.1-graalce..."
sdk install java 25.0.1-graalce

echo "Setting 25.0.1-graalce as default..."
sdk default java 25.0.1-graalce

echo "Verifying installation..."
java -version
