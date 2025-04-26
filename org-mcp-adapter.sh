#!/bin/bash
# org-mcp-adapter.sh
# Adapter script for connecting Claude Code to Emacs MCP via stdio

# Ensure Emacs server is running and start MCP server
emacsclient -e "(progn (require 'org-autotask-mcp) (org-autotask-mcp-start-server) t)" >/dev/null 2>&1 || {
    echo "Error: Cannot connect to Emacs server" >&2
    exit 1
}

# Process each line of input as a JSON-RPC request
while read -r line; do
    # Skip empty lines
    if [ -z "$line" ]; then
        continue
    fi
    
    # Escape quotes for elisp (replace " with \")
    escaped_line=$(echo "$line" | sed 's/"/\\"/g')
    
    emacsclient -e "(mcp-process-jsonrpc \"$escaped_line\")"
done

exit 0