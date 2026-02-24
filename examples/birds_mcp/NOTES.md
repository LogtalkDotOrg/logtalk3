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


# birds_mcp

This example demonstrates the `mcp_server` library by exposing the
bird identification expert system (from the `birds` example) as an
MCP (Model Context Protocol) server with **elicitation** support.

When an MCP client asks to identify a bird, the server uses the
`elicitation/create` MCP method to ask the user questions about bird
characteristics (yes/no questions and multiple-choice menus). The
answers guide the expert system through the bird taxonomy to identify
the species — exactly as the original interactive expert system does,
but over the MCP protocol instead of terminal I/O.

## Key concepts demonstrated

- Implementing `mcp_tool_protocol` with the `elicitation` capability
- Using `tool_call/4` with the `Elicit` closure for interactive tools
- Building JSON Schema for yes/no and enum elicitation requests
- Adapting an existing interactive application for MCP without
  modifying the original knowledge base

## Architecture

The example reuses the bird taxonomy from `examples/birds/` (the
`order` prototype hierarchy (the `descriptors` category, the `birds` objects).
The expert system's user interaction is reimplemented in `birds_mcp.lgt`:

- `birds_mcp` implements `mcp_tool_protocol` and declares the
  `elicitation` capability
- A single tool `identify_bird` is exposed
- The `check/2` predicate threads the `Elicit` closure through the
  identification logic
- `ask_descriptor/2` dispatches each bird descriptor to either a
  yes/no elicitation (`ask/3`) or a multiple-choice menu
  (`menuask/4`), based on `menu_attribute/2` facts
- Previous answers are memoized in `known_/3` to avoid redundant
  questions (matching the original expert system behavior)

The server reads/writes JSON-RPC 2.0 messages from/to stdin/stdout as
required by MCP.

## MCP client configuration

MCP client configuration depends on the MCP client. For example, to add
this MCP server to Claude Desktop or VSCode, edit (respectively) the
`claude_desktop_config.json` or `mcp.json` files and add:

```json
{
	"mcpServers": {
		"my-server": {
			"command": "swilgt",
			"args": [
                "-q",
				"-g", "logtalk_load(birds_mcp(server))",
				"-t", "halt"
			],
			"env": {
				"LOGTALKHOME": "/usr/local/share/logtalk",
				"LOGTALKUSER": "/Users/jdoe/logtalk"
			}
		}
	}
}
```

The `env` definition of the `LOGTALKHOME` and `LOGTALKUSER` environment
variables may or may not be required (it's usually necessary on macOS).
When required, replace the values above with the actual values on your
Logtalk setup.

The actual arguments to the integration script (`swilgt` in the example
above) depend on the Prolog backend.

## Protocol interaction example

A typical identification session involves the following MCP message
exchange:

1. Client sends `initialize` request
2. Server responds with capabilities including `elicitation`
3. Client sends `tools/list` request
4. Server responds with the `identify_bird` tool
5. Client sends `tools/call` for `identify_bird`
6. Server sends `elicitation/create` requests asking about bird
   characteristics (e.g., "eats: meat?", "What is the value for
   size?")
7. Client responds to each elicitation with the user's answer
8. Server returns the identification result

Example elicitation request (server -> client):

```json
{
  "jsonrpc": "2.0",
  "id": "elicit_1",
  "method": "elicitation/create",
  "params": {
    "message": "bill: sharp_hooked?",
    "requestedSchema": {
      "type": "object",
      "properties": {
        "answer": {"type": "string", "enum": ["yes", "no"]}
      },
      "required": ["answer"]
    }
  }
}
```

Client response:

```json
{
  "jsonrpc": "2.0",
  "id": "elicit_1",
  "result": {
    "action": "accept",
    "content": {"answer": "yes"}
  }
}
```
