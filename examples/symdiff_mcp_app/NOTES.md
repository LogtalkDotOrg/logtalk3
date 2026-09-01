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


`symdiff_mcp_app`
=================

This example demonstrates how to expose an existing Logtalk application as
an MCP App using the `mcp_server` library. It wraps the `symdiff` example in
an interactive HTML resource with separate launcher and differentiation tools.

An Apps-capable host renders the resource in a sandboxed iframe. The user can
enter an expression and see both its symbolic derivative and the simplified
derivative. A host without MCP Apps support can still call the tool and use
its text and structured results.


Supported expressions
---------------------

Expressions use the syntax accepted by the `symdiff` example:

- integer constants
- the variable `x`
- addition, subtraction, and multiplication using `+`, `-`, and `*`
- exponentiation using `**`
- logarithms using `log/1`

For example:

    2*x**3 + x**2 - 4*x

Input text is parsed using the portable `term_io` library and validated before
the parsed term receives a message. Syntax errors, variables other than the
atom `x`, unsupported functors, and expressions that cannot be differentiated
produce a structured error result.


Architecture
------------

The `symdiff_mcp_app` object implements three protocols:

| Protocol                | Role                                       |
|-------------------------|--------------------------------------------|
| `mcp_tool_protocol`     | Declares and handles `differentiate`       |
| `mcp_resource_protocol` | Serves the `ui://` HTML resource           |
| `mcp_ui_protocol`       | Links the tool to its interactive resource |

The model-facing `open_symdiff` tool takes no arguments and opens the app in
its idle state. The app-only `differentiate` tool requires an `expression`
string and returns structured content with these fields:

| Field        | Meaning                                      |
|--------------|----------------------------------------------|
| `status`     | `idle`, `success`, or `error`                 |
| `derivative` | Unsimplified derivative, or an empty string  |
| `simplified` | Simplified derivative, or an empty string    |
| `error`      | Error message, or an empty string            |

The launcher returns `idle`, allowing the initial UI to wait for user input
without displaying an error. Empty or missing calls made directly to the
app-only tool also return `idle`; non-empty invalid expressions return `error`.

The UI is entirely contained in `mcp-app.html`: markup, styling, and the
minimal JavaScript MCP Apps client. It has no external scripts, stylesheets,
SDK, or network assets. The client implements only the bridge operations used
by this example:

- `ui/initialize` and `ui/notifications/initialized`
- `tools/call`
- `ui/notifications/tool-result`
- `ui/notifications/host-context-changed`
- `ui/notifications/size-changed`


Server entry points
-------------------

| Specification / transport    | Entry point                  |
|------------------------------|------------------------------|
| 2025-06-18 (stdio)           | `server_2025_06_18.lgt`      |
| 2025-11-25 (stdio)           | `server_2025_11_25.lgt`      |
| 2026-07-28 (stdio)           | `server_2026_07_28.lgt`      |
| 2026-07-28 (Streamable HTTP) | `server_streamable_http.lgt` |

The same application object and HTML resource are used by every entry point.

The stdio entry points must be started by an MCP client. A typical client
configuration using the 2025-06-18 entry point using the SWI-Prolog backend
to exemplify is:

    {
      "mcpServers": {
        "symdiff": {
          "command": "swilgt",
          "args": [
            "-q",
            "-g", "logtalk_load(symdiff_mcp_app(server_2025_06_18))",
            "-t", "halt"
          ]
        }
      }
    }

Set `LOGTALKHOME` and `LOGTALKUSER` in the client environment when required
by the local Logtalk installation.

The Streamable HTTP server can be started directly using e.g. the SWI-Prolog
backend:

    $ swilgt -q -g "logtalk_load(symdiff_mcp_app(server_streamable_http))" -t halt

It listens by default at:

    http://127.0.0.1:8080/mcp

For other backends, see their documentation for equivalent command-line options.


Testing
-------

To run the server-side contract tests, load the tester file:

    | ?- logtalk_load(symdiff_mcp_app(tester)).

The tests cover tool metadata and schemas, UI metadata and resource content,
successful differentiation and simplification, and invalid input handling.
Rendering and iframe bridge behavior require an Apps-capable MCP host (e.g.,
MCPJam, Postman, VSCode, ...).
