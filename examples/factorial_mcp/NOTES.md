________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`factorial_mcp`
===============

This example demonstrates the `mcp_server` library by using a MCP (Model
Context Protocol) server providing a tool to compute the factorial of a
non-negative integer. It supports **three** entry points:

| Specification / transport    | Entry point                  |
|------------------------------|------------------------------|
| 2025-06-18 (stdio)           | `server_2025_06_18.lgt`      |
| 2026-07-28 (stdio)           | `server_2026_07_28.lgt`      |
| 2026-07-28 (Streamable HTTP) | `server_streamable_http.lgt` |


Testing
-------

To test this example server-side predicates, load the `tester.lgt` file:

    | ?- logtalk_load(factorial_mcp(tester)).

Tests only cover the server-side contract (capabilities, tool `_meta.ui`,
resource MIME type, and `tools/call` text result).


Starting the servers
--------------------

Only the Streamable HTTP example can (and must) be started from the
command-line (the stdio examples must be started from the clients).
For example, using the SWI-Prolog backend:

    $ swilgt -q -g "logtalk_load(factorial_mcp(server_streamable_http))" -t halt

Other backends provide similar command-line options; see their documentation
for details.

Default listen address (can be changed by editing the `http_port/1` and
`http_path/1` options in the `server_streamable_http` file):

http://127.0.0.1:8080/mcp

Example discovery request (2026):

    $ curl -sS -X POST 'http://127.0.0.1:8080/mcp' \
      -H 'Content-Type: application/json' \
      -H 'Accept: application/json, text/event-stream' \
      -H 'MCP-Protocol-Version: 2026-07-28' \
      -d '{
        "jsonrpc": "2.0",
        "id": 1,
        "method": "server/discover",
        "params": {
          "_meta": {
            "io.modelcontextprotocol/protocolVersion": "2026-07-28",
            "io.modelcontextprotocol/clientCapabilities": {
              "tools": {},
              "resources": {}
            }
          }
        }
      }'

Example tool call:

    curl -sS -X POST 'http://127.0.0.1:8080/mcp' \
      -H 'Content-Type: application/json' \
      -H 'Accept: application/json, text/event-stream' \
      -H 'MCP-Protocol-Version: 2026-07-28' \
      -d '{
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/call",
        "params": {
          "name": "get_time",
          "arguments": {},
          "_meta": {
            "io.modelcontextprotocol/protocolVersion": "2026-07-28",
            "io.modelcontextprotocol/clientCapabilities": {
              "tools": {},
              "resources": {}
            }
          }
        }
      }'


MCP client configuration
------------------------

Configuration illustrated using the SWI-Prolog backend. Updated for your
preferred backend (se its documentation for similar command-line options). 
Also update the values of the `LOGTALKHOME` and `LOGTALKUSER` environment
variables with their values on your system.

### 2025-06-18 (stdio)

    {
    	"mcpServers": {
    		"factorial-2025": {
    			"command": "swilgt",
    			"args": [
    				"-q",
    				"-g", "logtalk_load(factorial_mcp(server_2025_06_18))",
    				"-t", "halt"
    			],
    			"env": {
    				"LOGTALKHOME": "/usr/local/share/logtalk",
    				"LOGTALKUSER": "/Users/jdoe/logtalk"
    			}
    		}
    	}
    }

### 2026-07-28 (stdio)

    {
    	"mcpServers": {
    		"factorial-2026": {
    			"command": "swilgt",
    			"args": [
    				"-q",
    				"-g", "logtalk_load(factorial_mcp(server_2026_07_28))",
    				"-t", "halt"
    			],
    			"env": {
    				"LOGTALKHOME": "/usr/local/share/logtalk",
    				"LOGTALKUSER": "/Users/jdoe/logtalk"
    			}
    		}
    	}
    }

### Streamable HTTP

After starting `server_streamable_http.lgt`, point the client at:

http://127.0.0.1:8080/mcp

Use a **2026-capable** client for Streamable HTTP (for example MCPJam pinned
to 2026-07-28). Legacy clients that only speak `initialize` (such as current
VS Code MCP) should use the **2025-06-18 stdio** entry point instead.
