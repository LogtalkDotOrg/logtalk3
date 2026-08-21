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
MCP (Model Context Protocol) server.

It supports **three** entry points:

| Specification / transport | Interaction model | Entry point |
|---------------------------|-------------------|-------------|
| **2025-06-18** (stdio) | Synchronous `elicitation/create` via `tool_call/4` | `server_2025_06_18.lgt` |
| **2026-07-28** (stdio) | Multi-round `input_required` / `complete` via `tool_call_round/4` | `server_2026_07_28.lgt` |
| **2026-07-28** (Streamable HTTP) | Same 2026 protocol over HTTP POST + optional SSE | `server_streamable_http.lgt` |

When an MCP client asks to identify a bird, the server asks the user
questions about bird characteristics (yes/no and multiple-choice menus).
The answers guide the expert system through the bird taxonomy.


## Key concepts demonstrated

### 2025-06-18

- Implementing `mcp_tool_protocol` with a requirement for the client
  `elicitation` capability
- Using `tool_call/4` with the `Elicit` closure for interactive tools
- Building JSON Schema for yes/no and enum elicitation requests

### 2026-07-28 (stdio)

- Implementing `mcp_multiround_protocol` alongside `mcp_tool_protocol`
- Using `tool_call_round/4` that returns `input_required(Requests, State)`
  or `complete(Result)`
- Carrying known answers in opaque, JSON-friendly `requestState`
- Selecting the adapter with `protocol_adapter(mcp_server_2026_07_28_adapter)`

### 2026-07-28 (Streamable HTTP)

- Selecting `protocol_adapter(mcp_server_streamable_http_adapter)`
- HTTP options: `http_port/1`, `http_bind/1`, `http_path/1`, `http_origin_check/1`
- Stateless JSON-RPC over `POST` with required `MCP-Protocol-Version` header
- Optional SSE responses when the client supplies a `progressToken`
- Multi-round bird identification works unchanged (state in `requestState`)


## Architecture

The example reuses the bird taxonomy from `examples/birds/` (the
`order` prototype hierarchy and descriptors). User interaction is
implemented in `birds_mcp.lgt`:

- A single tool `identify_bird` is exposed
- **2025 path:** `tool_call/4` threads an `Elicit` closure through
  `ask/3` and `menuask/4`; answers are memoized in `known_/3`
- **2026 path:** `tool_call_round/4` asks one question per round,
  encodes known facts + pending question in `requestState`, and
  resumes from `inputResponses` on the next call
- **HTTP path:** same 2026 application code; only the transport adapter
  changes


## Testing

Run the stdio specification suites:

```text
| ?- logtalk_load(birds_mcp(tester)).
```

Streamable HTTP unit tests for the library adapter live in the
`mcp_server` library (`tests_streamable_http`). This example focuses on
stdio tests for the bird knowledge base; exercise HTTP with the server
entry point below and an HTTP client.


## Starting the servers

### 2025-06-18 (stdio)

```text
$ swilgt -q -g "logtalk_load(birds_mcp(server_2025_06_18))" -t halt
```

### 2026-07-28 (stdio)

```text
$ swilgt -q -g "logtalk_load(birds_mcp(server_2026_07_28))" -t halt
```

### Streamable HTTP

```text
$ swilgt -q -g "logtalk_load(birds_mcp(server_streamable_http))"
```

Default listen address: `http://127.0.0.1:8080/mcp`.

Example discovery request:

```bash
curl -sS -X POST 'http://127.0.0.1:8080/mcp' \
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
          "elicitation": {}
        }
      }
    }
  }'
```

First identification round:

```bash
curl -sS -X POST 'http://127.0.0.1:8080/mcp' \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H 'MCP-Protocol-Version: 2026-07-28' \
  -d '{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {
      "name": "identify_bird",
      "arguments": {},
      "_meta": {
        "io.modelcontextprotocol/protocolVersion": "2026-07-28",
        "io.modelcontextprotocol/clientCapabilities": {
          "tools": {},
          "elicitation": {}
        }
      }
    }
  }'
```

The response is `resultType: input_required` with `inputRequests` and
`requestState`. Send the next `tools/call` with `inputResponses` and the
echoed `requestState` until `resultType: complete`.


## MCP client configuration

### 2025-06-18 (stdio)

```json
{
	"mcpServers": {
		"birds-expert-2025": {
			"command": "swilgt",
			"args": [
				"-q",
				"-g", "logtalk_load(birds_mcp(server_2025_06_18))",
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

### 2026-07-28 (stdio)

```json
{
	"mcpServers": {
		"birds-expert-2026": {
			"command": "swilgt",
			"args": [
				"-q",
				"-g", "logtalk_load(birds_mcp(server_2026_07_28))",
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

### Streamable HTTP

HTTP MCP clients typically point at a URL rather than a command. After
starting `server_streamable_http.lgt`, configure the client with:

```text
http://127.0.0.1:8080/mcp
```

Headers required on each request:

- `MCP-Protocol-Version: 2026-07-28`
- `Content-Type: application/json`
- `Accept: application/json, text/event-stream` (recommended)

Replace `LOGTALKHOME` / `LOGTALKUSER` with the values on your system
when required (often needed on macOS).


## Protocol interaction examples

### 2025-06-18 (synchronous elicitation)

1. Client sends `initialize` advertising the `elicitation` capability
2. Server responds with the `tools` capability
3. Client calls `tools/call` for `identify_bird`
4. Server sends one or more `elicitation/create` requests
5. Client answers each; server returns the identification result

### 2026-07-28 (multi-round, stdio or HTTP)

1. Client sends `server/discover` with required `_meta`
2. Client calls `tools/call` for `identify_bird`
3. Server responds with `resultType: input_required`, an
   `inputRequests` list, and `requestState`
4. Client calls `tools/call` again with `inputResponses` and the
   echoed `requestState`
5. Steps 3–4 repeat until the server returns `resultType: complete`

Over HTTP, each step is a separate `POST` to `/mcp`. Over stdio, the
same JSON-RPC messages are newline-delimited on the process streams.


## Files

| File | Role |
|------|------|
| `birds_mcp.lgt` | Tool provider (2025 elicitation + 2026 MRTR) |
| `server_2025_06_18.lgt` | 2025-06-18 stdio server entry point |
| `server_2026_07_28.lgt` | 2026-07-28 stdio server entry point |
| `server_streamable_http.lgt` | 2026-07-28 Streamable HTTP server entry point |
| `loader.lgt` | Example loader |
| `tests_2025_06_18.lgt` | 2025-06-18 unit tests |
| `tests_2026_07_28.lgt` | 2026-07-28 MRTR unit tests |
| `tester.lgt` | Runs both stdio test suites |
