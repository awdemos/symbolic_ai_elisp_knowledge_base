# MCP Server for Symbolic AI Knowledge Base

Model Context Protocol (MCP) server exposing the Emacs Lisp KB system as tools for LLM integration.

## Usage

### Manual Mode (Recommended)

The server runs in manual JSON-RPC mode by default (or set `MCP_MANUAL=1`):

```bash
python3 mcp/kb_mcp_server.py
```

### With MCP SDK

If the `mcp` Python package is installed, the server can use the official SDK:

```bash
python3 mcp/kb_mcp_server.py
```

## Tools

- **kb_query** - Query facts from the knowledge base
- **kb_assert** - Assert a new fact
- **kb_reason** - Run inference/reasoning
- **kb_status** - Get KB status
- **kb_init** - Initialize/reset the KB

## Example

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"kb_query","arguments":{"subject":"Marie_Curie","predicate":"discovered"}}}' | MCP_MANUAL=1 python3 mcp/kb_mcp_server.py
```

## Integration

Add to your MCP client configuration (e.g., Claude Desktop):

```json
{
  "mcpServers": {
    "knowledge-base": {
      "command": "python3",
      "args": ["/path/to/mcp/kb_mcp_server.py"],
      "env": {
        "MCP_MANUAL": "1"
      }
    }
  }
}
```
