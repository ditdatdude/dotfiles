#!/bin/bash
# Simple WSL clipboard functions

copy() {
    if [[ $# -eq 0 ]]; then
        cat | clip.exe
    else
        echo -n "$*" | clip.exe
    fi
    echo "Copied to clipboard"
}

paste() {
    powershell.exe -command 'Get-Clipboard' 2>/dev/null | sed 's/\r$//'
}

test_clipboard() {
    echo "test" | copy
    local result=$(paste)
    if [[ "$result" == "test" ]]; then
        echo "Clipboard working"
    else
        echo "Clipboard failed"
    fi
}
