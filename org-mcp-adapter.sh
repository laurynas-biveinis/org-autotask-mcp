#!/bin/bash
# org-mcp-adapter.sh
# Adapter script for connecting Claude Code to Emacs MCP via stdio

# Ensure Emacs server is running
emacsclient -e "(progn (require 'org-autotask-mcp) t)" >/dev/null 2>&1 || {
    echo "Error: Cannot connect to Emacs server" >&2
    exit 1
}

# Start the MCP server if not already running
emacsclient -e "(progn (require 'org-autotask-mcp) (org-autotask-mcp-start-server) t)" >/dev/null

# Process each line of input as a JSON-RPC request
while read -r line; do
    # Skip empty lines
    if [ -z "$line" ]; then
        continue
    fi
    
    # Escape quotes for elisp (replace " with \")
    escaped_line=$(echo "$line" | sed 's/"/\\"/g')
    
    # Call Emacs to process the request via mcp-process-jsonrpc
    result=$(emacsclient -e "(mcp-process-jsonrpc \"$escaped_line\")")
    
    # Check if we have a result
    if [ -n "$result" ] && [ "$result" != "nil" ]; then
        # Remove surrounding quotes from the result (emacsclient returns strings quoted)
        result="${result#\"}"
        result="${result%\"}"
        
        # Two-step approach to handle escaping properly:
        # 1. First handle special content with double-escaped quotes (\\\" to \") 
        result=$(echo "$result" | sed 's/\\\\"/\\"/g')
        
        # 2. Then process JSON structure quotes (\") 
        result=$(echo "$result" | sed 's/\\"/"/g')
        
        # Output the properly formatted JSON
        echo "$result"
    fi
done

exit 0