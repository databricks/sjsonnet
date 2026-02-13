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

# --- Go & Starlark setup ---
GO_VERSION="1.23.4"
GO_TAR="go${GO_VERSION}.linux-amd64.tar.gz"
INSTALL_DIR="$HOME/.local/go"

if [ ! -d "$INSTALL_DIR" ] || [ "$($INSTALL_DIR/bin/go version | awk '{print $3}')" != "go$GO_VERSION" ]; then
    echo "Installing Go $GO_VERSION..."
    mkdir -p "$HOME/tmp"
    curl -L "https://golang.org/dl/$GO_TAR" -o "$HOME/tmp/$GO_TAR"
    rm -rf "$INSTALL_DIR"
    mkdir -p "$INSTALL_DIR"
    tar -C "$INSTALL_DIR" --strip-components=1 -xzf "$HOME/tmp/$GO_TAR"
    rm "$HOME/tmp/$GO_TAR"
else
    echo "Go $GO_VERSION already installed."
fi

export PATH="$INSTALL_DIR/bin:$PATH"
echo "Go version: $(go version)"

echo "Installing Starlark Go interpreter..."
# Use the official module path for installation
go install go.starlark.net/cmd/starlark@latest

export PATH="$HOME/go/bin:$PATH"
if command -v starlark &> /dev/null; then
    echo "Starlark Go interpreter installed successfully."
else
    echo "Starlark Go installation failed or PATH not updated."
fi
