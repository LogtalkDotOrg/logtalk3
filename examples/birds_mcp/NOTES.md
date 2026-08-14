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

It supports **both** specifications:

| Specification | Interaction model | Entry point |
|---------------|-------------------|-------------|
| **2025-06-18** | Synchronous `elicitation/create` via `tool_call/4` | `server.lgt` (default) |
| **2026-07-28** | Multi-round `input_required` / `complete` via `tool_call_round/4` | `server_2026_07_28.lgt` |

When an MCP client asks to identify a bird, the server asks the user
questions about bird characteristics (yes/no and multiple-choice menus).
The answers guide the expert system through the bird taxonomy.


## Key concepts demonstrated

### 2025-06-18

- Implementing `mcp_tool_protocol` with a requirement for the client
  `elicitation` capability
- Using `tool_call/4` with the `Elicit` closure for interactive tools
- Building JSON Schema for yes/no and enum elicitation requests

### 2026-07-28

- Implementing `mcp_multiround_protocol` alongside `mcp_tool_protocol`
- Using `tool_call_round/4` that returns `input_required(Requests, State)`
  or `complete(Result)`
- Carrying known answers in opaque, JSON-friendly `requestState`
- Selecting the adapter with `protocol_adapter(mcp_server_2026_07_28_adapter)`


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


## Testing

Run both specification suites:

```text
| ?- logtalk_load(birds_mcp(tester)).
```


## MCP client configuration

### 2025-06-18 (default)

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

### 2026-07-28

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

Replace `LOGTALKHOME` / `LOGTALKUSER` with the values on your system
when required (often needed on macOS).


## Protocol interaction examples

### 2025-06-18 (synchronous elicitation)

1. Client sends `initialize` advertising the `elicitation` capability
2. Server responds with the `tools` capability
3. Client calls `tools/call` for `identify_bird`
4. Server sends one or more `elicitation/create` requests
5. Client answers each; server returns the identification result

### 2026-07-28 (multi-round)

1. Client sends `server/discover` with required `_meta`
2. Client calls `tools/call` for `identify_bird`
3. Server responds with `resultType: input_required`, an
   `inputRequests` list, and `requestState`
4. Client calls `tools/call` again with `inputResponses` and the
   echoed `requestState`
5. Steps 3–4 repeat until the server returns `resultType: complete`


## Files

| File | Role |
|------|------|
| `birds_mcp.lgt` | Tool provider (2025 elicitation + 2026 MRTR) |
| `server.lgt` | 2025-06-18 server entry point |
| `server_2026_07_28.lgt` | 2026-07-28 server entry point |
| `loader.lgt` | Example loader |
| `tests.lgt` | 2025-06-18 unit tests |
| `tests_2026_07_28.lgt` | 2026-07-28 MRTR unit tests |
| `tester.lgt` | Runs both test suites |
