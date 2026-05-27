#!/usr/bin/env python3
"""
MCP Server for Symbolic AI Knowledge Base.

Exposes the Emacs Lisp KB system as MCP tools for LLM integration.
Uses stdio transport with JSON-RPC 2.0.
"""

import json
import subprocess
import sys
import os
from typing import Any, Dict, List, Optional

try:
    from mcp.server import Server
    from mcp.server.stdio import stdio_server
    from mcp.types import TextContent, Tool
    HAS_MCP_SDK = True
except ImportError:
    HAS_MCP_SDK = False
    print("Warning: MCP SDK not available, using manual JSON-RPC", file=sys.stderr)


class KBEmacsInterface:
    def __init__(self, lisp_dir: Optional[str] = None):
        if lisp_dir is None:
            script_dir = os.path.dirname(os.path.abspath(__file__))
            possible_paths = [
                os.path.join(script_dir, "..", "lisp"),
                os.path.join(script_dir, "..", "..", "lisp"),
                os.path.join(os.getcwd(), "lisp"),
            ]
            for path in possible_paths:
                if os.path.exists(os.path.join(path, "kb-advanced-system.el")):
                    lisp_dir = path
                    break
        
        self.lisp_dir = lisp_dir
        if not self.lisp_dir or not os.path.exists(self.lisp_dir):
            raise RuntimeError(f"Cannot find KB lisp directory")
    
    def _run_elisp(self, elisp_code: str) -> str:
        cmd = [
            "emacs", "--batch",
            "--eval", f"(add-to-list 'load-path \"{self.lisp_dir}\")",
            "--eval", "(require 'kb-advanced-system)",
            "--eval", "(kb-init)",
            "--eval", elisp_code,
        ]
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
            
            lines = result.stdout.strip().split('\n')
            output_lines = []
            for line in lines:
                if line.startswith('KB Advanced system') or line.startswith('Added default'):
                    continue
                if line.startswith('All defaults') or line.startswith('KB state cleared'):
                    continue
                output_lines.append(line)
            
            return '\n'.join(output_lines)
        except subprocess.TimeoutExpired:
            return "Error: Emacs execution timed out"
        except Exception as e:
            return f"Error: {str(e)}"
    
    def query(self, subject: str, predicate: str, microtheory: Optional[str] = None) -> Dict[str, Any]:
        mt_arg = f"'{microtheory}" if microtheory else "nil"
        elisp = f'(prin1 (kb-query \'{subject} \'{predicate} {mt_arg}))'
        result = self._run_elisp(elisp)
        
        try:
            parsed = self._parse_lisp_value(result)
            return {
                "subject": subject,
                "predicate": predicate,
                "microtheory": microtheory,
                "results": parsed if parsed else [],
                "count": len(parsed) if parsed else 0,
                "grounded": bool(parsed)
            }
        except Exception as e:
            return {
                "subject": subject,
                "predicate": predicate,
                "error": str(e),
                "raw_output": result
            }
    
    def assert_fact(self, subject: str, predicate: str, obj: str, 
                    certainty: float = 0.9, microtheory: Optional[str] = None) -> Dict[str, Any]:
        mt_arg = f"'{microtheory}" if microtheory else "nil"
        elisp = f'(prin1 (kb-assert \'{subject} \'{predicate} \'{obj} {certainty} nil {mt_arg}))'
        result = self._run_elisp(elisp)
        
        return {
            "subject": subject,
            "predicate": predicate,
            "object": obj,
            "certainty": certainty,
            "microtheory": microtheory,
            "success": "Error" not in result,
            "result": result
        }
    
    def reason(self, microtheory: Optional[str] = None) -> Dict[str, Any]:
        mt_arg = f"'{microtheory}" if microtheory else "nil"
        elisp = f'(prin1 (kb-reason {mt_arg}))'
        result = self._run_elisp(elisp)
        
        return {
            "microtheory": microtheory,
            "result": result
        }
    
    def status(self) -> Dict[str, Any]:
        elisp = '(prin1 (kb-status))'
        result = self._run_elisp(elisp)
        
        return {
            "status": result
        }
    
    def _parse_lisp_value(self, value: str) -> Any:
        value = value.strip()
        if not value or value == "nil":
            return []
        
        try:
            return json.loads(value)
        except:
            pass
        
        if value.startswith('(') and value.endswith(')'):
            inner = value[1:-1].strip()
            if not inner:
                return []
            
            items = []
            current = ""
            depth = 0
            for char in inner:
                if char == '(':
                    depth += 1
                    current += char
                elif char == ')':
                    depth -= 1
                    current += char
                elif char == ' ' and depth == 0:
                    if current:
                        items.append(self._parse_atom(current))
                        current = ""
                else:
                    current += char
            
            if current:
                items.append(self._parse_atom(current))
            
            return items
        
        return self._parse_atom(value)
    
    def _parse_atom(self, atom: str) -> Any:
        atom = atom.strip()
        
        if atom.startswith("'"):
            return atom[1:]
        
        if atom == "t":
            return True
        if atom == "nil":
            return None
        
        try:
            if '.' in atom:
                return float(atom)
            return int(atom)
        except ValueError:
            pass
        
        if atom.startswith('"') and atom.endswith('"'):
            return atom[1:-1]
        
        return atom


class ManualMCPServer:
    def __init__(self, kb: KBEmacsInterface):
        self.kb = kb
        self.tools = {
            "kb_query": self._handle_query,
            "kb_assert": self._handle_assert,
            "kb_reason": self._handle_reason,
            "kb_status": self._handle_status,
            "kb_init": self._handle_init,
        }
    
    def run(self):
        self._send({
            "jsonrpc": "2.0",
            "id": 0,
            "result": {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "serverInfo": {"name": "kb-mcp-server", "version": "1.0.0"}
            }
        })
        
        while True:
            try:
                line = input()
                if not line:
                    continue
                
                request = json.loads(line)
                self._handle_request(request)
            except EOFError:
                break
            except Exception as e:
                self._send({
                    "jsonrpc": "2.0",
                    "id": None,
                    "error": {"code": -32603, "message": f"Internal error: {str(e)}"}
                })
    
    def _handle_request(self, request: Dict):
        method = request.get("method", "")
        req_id = request.get("id")
        params = request.get("params", {})
        
        if method == "initialize":
            self._send({
                "jsonrpc": "2.0",
                "id": req_id,
                "result": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {"tools": {}},
                    "serverInfo": {"name": "kb-mcp-server", "version": "1.0.0"}
                }
            })
        elif method == "tools/list":
            self._send({
                "jsonrpc": "2.0",
                "id": req_id,
                "result": {
                    "tools": [
                        {
                            "name": "kb_query",
                            "description": "Query facts from the knowledge base",
                            "inputSchema": {
                                "type": "object",
                                "properties": {
                                    "subject": {"type": "string", "description": "Entity to query about"},
                                    "predicate": {"type": "string", "description": "Relationship to query"},
                                    "microtheory": {"type": "string", "description": "Optional microtheory context"}
                                },
                                "required": ["subject", "predicate"]
                            }
                        },
                        {
                            "name": "kb_assert",
                            "description": "Assert a new fact into the knowledge base",
                            "inputSchema": {
                                "type": "object",
                                "properties": {
                                    "subject": {"type": "string", "description": "Entity"},
                                    "predicate": {"type": "string", "description": "Relationship"},
                                    "object": {"type": "string", "description": "Target value"},
                                    "certainty": {"type": "number", "description": "Confidence (0.0-1.0)"},
                                    "microtheory": {"type": "string", "description": "Optional microtheory context"}
                                },
                                "required": ["subject", "predicate", "object"]
                            }
                        },
                        {
                            "name": "kb_reason",
                            "description": "Run inference/reasoning on the knowledge base",
                            "inputSchema": {
                                "type": "object",
                                "properties": {
                                    "microtheory": {"type": "string", "description": "Optional microtheory context"}
                                }
                            }
                        },
                        {
                            "name": "kb_status",
                            "description": "Get knowledge base status and statistics",
                            "inputSchema": {
                                "type": "object",
                                "properties": {}
                            }
                        },
                        {
                            "name": "kb_init",
                            "description": "Initialize or reset the knowledge base",
                            "inputSchema": {
                                "type": "object",
                                "properties": {}
                            }
                        }
                    ]
                }
            })
        elif method == "tools/call":
            tool_name = params.get("name", "")
            tool_args = params.get("arguments", {})
            
            if tool_name in self.tools:
                try:
                    result = self.tools[tool_name](tool_args)
                    self._send({
                        "jsonrpc": "2.0",
                        "id": req_id,
                        "result": {
                            "content": [
                                {
                                    "type": "text",
                                    "text": json.dumps(result, indent=2)
                                }
                            ]
                        }
                    })
                except Exception as e:
                    self._send({
                        "jsonrpc": "2.0",
                        "id": req_id,
                        "error": {"code": -32602, "message": f"Tool error: {str(e)}"}
                    })
            else:
                self._send({
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "error": {"code": -32601, "message": f"Unknown tool: {tool_name}"}
                })
        else:
            self._send({
                "jsonrpc": "2.0",
                "id": req_id,
                "error": {"code": -32601, "message": f"Unknown method: {method}"}
            })
    
    def _handle_query(self, args: Dict) -> Dict:
        return self.kb.query(args["subject"], args["predicate"], args.get("microtheory"))
    
    def _handle_assert(self, args: Dict) -> Dict:
        return self.kb.assert_fact(
            args["subject"], args["predicate"], args["object"],
            args.get("certainty", 0.9), args.get("microtheory")
        )
    
    def _handle_reason(self, args: Dict) -> Dict:
        return self.kb.reason(args.get("microtheory"))
    
    def _handle_status(self, args: Dict) -> Dict:
        return self.kb.status()
    
    def _handle_init(self, args: Dict) -> Dict:
        return {"status": "initialized"}
    
    def _send(self, msg: Dict):
        print(json.dumps(msg), flush=True)


async def main_sdk():
    kb = KBEmacsInterface()
    server = Server("kb-mcp-server")
    
    @server.list_tools()
    async def list_tools() -> List[Tool]:
        return [
            Tool(
                name="kb_query",
                description="Query facts from the knowledge base",
                inputSchema={
                    "type": "object",
                    "properties": {
                        "subject": {"type": "string", "description": "Entity to query about"},
                        "predicate": {"type": "string", "description": "Relationship to query"},
                        "microtheory": {"type": "string", "description": "Optional microtheory context"}
                    },
                    "required": ["subject", "predicate"]
                }
            ),
            Tool(
                name="kb_assert",
                description="Assert a new fact into the knowledge base",
                inputSchema={
                    "type": "object",
                    "properties": {
                        "subject": {"type": "string", "description": "Entity"},
                        "predicate": {"type": "string", "description": "Relationship"},
                        "object": {"type": "string", "description": "Target value"},
                        "certainty": {"type": "number", "description": "Confidence (0.0-1.0)"},
                        "microtheory": {"type": "string", "description": "Optional microtheory context"}
                    },
                    "required": ["subject", "predicate", "object"]
                }
            ),
            Tool(
                name="kb_reason",
                description="Run inference/reasoning on the knowledge base",
                inputSchema={
                    "type": "object",
                    "properties": {
                        "microtheory": {"type": "string", "description": "Optional microtheory context"}
                    }
                }
            ),
            Tool(
                name="kb_status",
                description="Get knowledge base status and statistics",
                inputSchema={
                    "type": "object",
                    "properties": {}
                }
            ),
        ]
    
    @server.call_tool()
    async def call_tool(name: str, arguments: Dict) -> List[TextContent]:
        if name == "kb_query":
            result = kb.query(arguments["subject"], arguments["predicate"], arguments.get("microtheory"))
            return [TextContent(type="text", text=json.dumps(result, indent=2))]
        elif name == "kb_assert":
            result = kb.assert_fact(
                arguments["subject"], arguments["predicate"], arguments["object"],
                arguments.get("certainty", 0.9), arguments.get("microtheory")
            )
            return [TextContent(type="text", text=json.dumps(result, indent=2))]
        elif name == "kb_reason":
            result = kb.reason(arguments.get("microtheory"))
            return [TextContent(type="text", text=json.dumps(result, indent=2))]
        elif name == "kb_status":
            result = kb.status()
            return [TextContent(type="text", text=json.dumps(result, indent=2))]
        else:
            raise ValueError(f"Unknown tool: {name}")
    
    async with stdio_server() as (read_stream, write_stream):
        await server.run(read_stream, write_stream)


def main():
    if os.environ.get("MCP_MANUAL") or not HAS_MCP_SDK:
        kb = KBEmacsInterface()
        server = ManualMCPServer(kb)
        server.run()
    else:
        import asyncio
        try:
            asyncio.run(main_sdk())
        except Exception as e:
            print(f"SDK mode failed ({e}), falling back to manual", file=sys.stderr)
            kb = KBEmacsInterface()
            server = ManualMCPServer(kb)
            server.run()


if __name__ == "__main__":
    main()
