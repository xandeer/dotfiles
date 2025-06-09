#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
XCODE_DIR="$HOME/Library/Developer/Xcode/UserData"
KEYBINDINGS_DIR="$XCODE_DIR/KeyBindings"
SNIPPETS_DIR="$XCODE_DIR/CodeSnippets"

mkdir -p "$KEYBINDINGS_DIR"

ln -sf "$SCRIPT_DIR"/KeyBindings/*.idekeybindings "$KEYBINDINGS_DIR"
ln -sf "$SCRIPT_DIR"/CodeSnippets "$SNIPPETS_DIR"
