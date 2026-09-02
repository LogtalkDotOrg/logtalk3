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


`get_time_mcp_app`
==================

This example demonstrates **MCP Apps** support in the `mcp_server` library:
an MCP tool linked to an interactive HTML UI resource (`ui://` scheme, MIME
type `text/html;profile=mcp-app`).

It exposes a single tool, `get_time`, that returns the current server time.
The tool declares `_meta.ui.resourceUri` pointing at a self-contained HTML
view. Hosts that implement the MCP Apps extension render that view in a
sandboxed iframe; hosts that do not still receive the normal text tool
result.

It supports **four** entry points:

| Specification / transport    | Interaction model                          | Entry point                        |
|------------------------------|--------------------------------------------|------------------------------------|
| 2025-06-18 (stdio)           | Tool call + optional Apps UI               | `server_2025_06_18.lgt`            |
| 2026-07-28 (stdio)           | Tool call + optional Apps UI               | `server_2026_07_28.lgt`            |
| 2026-07-28 (Streamable HTTP) | Same protocol over HTTP POST + optional SSE| `server_streamable_http.lgt`       |
| 2026-07-28 (OAuth HTTPS)     | Protected HTTPS plus public OAuth metadata | `server_streamable_http_oauth.lgt` |

MCP Apps is transport- and core-revision-agnostic: the same application
object works on both protocol versions and on both stdio and Streamable
HTTP.


Key concepts demonstrated
-------------------------

- Declaring the MCP Apps extension by including `ui` (and `resources`) in
  `capabilities/1`, which advertises
  `extensions["io.modelcontextprotocol/ui"]` with MIME
  `text/html;profile=mcp-app`
- Implementing `mcp_tool_protocol`, `mcp_resource_protocol`, and
  `mcp_ui_protocol`
- Linking a tool to a UI resource with `tool_ui/2`
  (`resource_uri/1`, `visibility/1`)
- Serving a `ui://` resource via `resources/1` and `resource_read/3`
- Optional resource CSP / border hints via `resource_ui_meta/2`
- A **minimal custom** Apps View client (JSON-RPC over `postMessage`)
  instead of embedding the full `@modelcontextprotocol/ext-apps` SDK
- Protecting an MCP Streamable HTTP server using the `mcp_server` `oauth/4`
  option and a verifier implementing `http_oauth_verifier_protocol`


UI: inspiration, minimal client, and license
--------------------------------------------

The UI design and interaction pattern are **inspired by** the official MCP
Apps quickstart and `basic-server-vanillajs` example:

- Repository: https://github.com/modelcontextprotocol/ext-apps  
- Quickstart: https://github.com/modelcontextprotocol/ext-apps/blob/main/docs/quickstart.md  
- Vanilla JS example: https://github.com/modelcontextprotocol/ext-apps/tree/main/examples/basic-server-vanillajs  
- Apps specification: https://github.com/modelcontextprotocol/ext-apps/blob/main/specification/2026-01-26/apps.mdx  
- Overview: https://modelcontextprotocol.io/extensions/apps/overview  

Upstream licensing (for reference when comparing with official samples):

- https://github.com/modelcontextprotocol/ext-apps/blob/main/LICENSE  
  (primarily Apache-2.0; some contributions MIT; docs CC-BY-4.0)  
- Copyright (c) 2024–2025 Model Context Protocol, a Series of LF Projects, LLC,
  and contributors

This example does **not** ship the official client bundle and does **not**
load the SDK from a CDN. The View is a small, self-contained HTML file with
hand-written JavaScript that implements only what this demo needs:

| Capability               | How it is done                                    |
|--------------------------|---------------------------------------------------|
| Handshake                | `ui/initialize` -> `ui/notifications/initialized` |
| Host -> View tool result | `ui/notifications/tool-result`                    |
| View -> server tool call | `tools/call` (proxied by the host)                |
| Optional chat line       | `ui/message` after a successful button refresh    |
| Light layout feedback    | `ui/notifications/size-changed`                   |

**Why minimal custom code?**

- **File size** — a full Vite-inlined `@modelcontextprotocol/ext-apps` build is
  on the order of hundreds of kilobytes; the minimal View is a few kilobytes.
- **No CDN** — the iframe does not fetch scripts from `esm.sh` / `unpkg`, so
  it is not blocked when the sandbox disallows third-party scripts.
- **No extra CSP `resourceDomains` for the SDK** — you only need CSP entries
  if the HTML itself loads external assets (this demo does not).
- **Clear teaching surface** — the `postMessage` JSON-RPC flow is visible in
  one file, which matches the Apps spec statement that the `App` class is a
  convenience wrapper, not a requirement.

Trade-off: the minimal client omits most of the official SDK (rich theming
helpers, `openLink`, logging helpers, display-mode requests, streaming
tool-input, and so on). Hosts that implement the Apps bridge should still
accept this subset; if a host is strict about optional fields, check the
iframe console for `ui/initialize` / `tools/call` errors.

The tool name used in the View must match `tools/list` exactly: **`get_time`**.

If the SDK was used, the example would benefit from defining the optional
`resource_ui_meta/2` predicate:

	% Optional CSP so the iframe may load the Apps SDK from esm.sh
	resource_ui_meta('ui://get-time/mcp-app.html', {
		csp-{
			resourceDomains-['https://esm.sh']
		},
		prefersBorder- @true
	}).


Architecture
------------

| Component           | Role                                                          |
|---------------------|---------------------------------------------------------------|
| `get_time`          | Application object: tool, UI resource, Apps metadata          |
| `mcp-app.html`      | Minimal Apps View (custom `postMessage` client)               |
| Server entry points | Select `spec/1` and `transport/1`, call `mcp_server::start/3` |

Flow when an Apps-capable host is used:

1. Client initializes / discovers; server advertises `tools`, `resources`,
   and the `io.modelcontextprotocol/ui` extension
2. Client lists tools; `get_time` includes `_meta.ui.resourceUri`
3. Client calls `get_time` and/or reads `ui://get-time/mcp-app.html`
4. Host renders the HTML in a sandboxed iframe; the View completes
   `ui/initialize` and listens for `ui/notifications/tool-result`
5. The UI may call `get_time` again with `tools/call` through the host


Prompt vs button: what updates where
------------------------------------

There are two ways the tool is invoked. They share the same server handler
but the **host** treats them differently.

### Chat prompt (model calls `get_time`)

1. The host runs `tools/call` for `get_time`.
2. The server returns text content (ISO-8601 time).
3. The host pushes that result into the iframe
   (`ui/notifications/tool-result`); the UI updates `#server-time`.
4. The host/model often also shows a normal chat line under the widget
   (for example “The current time is …”). That text is host/LLM behaviour,
   not something the MCP server controls.

### **Get Server Time** button (iframe calls the tool)

1. The UI sends `tools/call` for `get_time` over `postMessage`.
2. The host proxies the call to the server and returns the result **to the
   iframe only**.
3. The UI updates `#server-time` from the result (it accepts both
   `structuredContent.time` and `content[].text`, so Logtalk `text(Time)`
   works).
4. By design, this path usually **does not** start a new model turn, so no
   new assistant line appears under the widget unless the host is told to
   show one.

After a successful button refresh, the UI also tries:

	ui/message
	  role: user
	  content: [{ type: text, text: "Updated server time: …" }]

If the host accepts `ui/message`, a line may appear in the conversation.
If the host ignores or rejects it, only the in-widget time changes; the
console may log that the message was not accepted. **Whether chat text
appears on button click is entirely host-dependent** (MCPJam, Claude
Desktop, etc.).

| Trigger               | `#server-time` in the UI               | Text under the widget                        |
|-----------------------|----------------------------------------|----------------------------------------------|
| Prompt / model call   | Updated via `tool-result` notification | Often yes (host/model)                       |
| Button / `tools/call` | Updated from tool result               | Only if host handles `ui/message` (optional) |


Testing
-------

To test this example server-side predicates, load the `tester.lgt` file:

    | ?- logtalk_load(get_time_mcp_app(tester)).

Tests only cover the server-side Apps contract (capabilities, tool `_meta.ui`,
resource MIME type, and `tools/call` text result). The full host <-> iframe 
behaviour testing requires an Apps-capable host.


Starting the servers
--------------------

Only the Streamable HTTP examples can (and must) be started from the
command-line (the stdio examples must be started from the clients).
For example, using the SWI-Prolog backend:

    $ swilgt -q -g "logtalk_load(get_time_mcp_app(server_streamable_http))" -t halt

To run the OAuth-protected variant instead:

    $ swilgt -q -g "logtalk_load(get_time_mcp_app(server_streamable_http_oauth))" -t halt

Other backends provide similar command-line options; see their documentation
for details.

Default listen address (can be changed by editing the `http_port/1` and
`http_path/1` options in the Streamable HTTP server entry-point files):

http://127.0.0.1:8080/mcp

Example discovery request (2026):

    $ curl -sS -X POST 'http://127.0.0.1:8080/mcp' \
      -H 'Content-Type: application/json' \
      -H 'Accept: application/json, text/event-stream' \
      -H 'MCP-Protocol-Version: 2026-07-28' \
	  -H 'Mcp-Method: server/discover' \
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
	  -H 'Mcp-Method: tools/call' \
	  -H 'Mcp-Name: get_time' \
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


    OAuth-protected variant
    -----------------------

    The protected entry point uses `get_time_oauth_verifier`, a deliberately
    simple verifier that accepts the fixed Bearer token `get-time-demo-token`
    with the `get_time` scope. This verifier exists only to keep the example
    self-contained. Production servers should use a JWT or introspection
    verifier from the `http_oauth` library.

    The configured protected-resource identifier is
    `https://127.0.0.1:8443/mcp`. The example uses
    `http_server_options([scheme(https), temporary_tls_credentials(...)])` to
    create temporary credentials for local testing. This requires the TLS helper
    programs documented by the `http_server` library. A deployed server should
    instead use `tls_certificate_file/1` and `tls_key_file/1` with its real
    certificate and private key.

    The RFC 9728 metadata endpoint remains public:

      $ curl -k -sS \
        'https://127.0.0.1:8443/.well-known/oauth-protected-resource/mcp'

    Requests to the MCP endpoint require the demo token. Add this header to the
    discovery and tool-call examples above:

      -H 'Authorization: Bearer get-time-demo-token'

    A request without that header receives `401 Unauthorized`; a valid token
    without the required `get_time` scope would receive `403 Forbidden`.


MCP client configuration
------------------------

Update the values of the `LOGTALKHOME` and `LOGTALKUSER` environment variables
with the values on your system (often needed on macOS).

### 2025-06-18 (stdio)

    {
    	"mcpServers": {
    		"get-time-2025": {
    			"command": "swilgt",
    			"args": [
    				"-q",
    				"-g", "logtalk_load(get_time_mcp_app(server_2025_06_18))",
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
    		"get-time-2026": {
    			"command": "swilgt",
    			"args": [
    				"-q",
    				"-g", "logtalk_load(get_time_mcp_app(server_2026_07_28))",
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

For `server_streamable_http_oauth.lgt`, configure the client to send
`Authorization: Bearer get-time-demo-token` and connect to:

https://127.0.0.1:8443/mcp

The temporary certificate is self-signed, so the client must allow it for
local testing. The exact settings for a fixed Bearer token and self-signed
certificate are client-specific.

Use a **2026-capable** client for Streamable HTTP (for example MCPJam pinned
to 2026-07-28). Legacy clients that only speak `initialize` (such as current
VS Code MCP) should use the **2025-06-18 stdio** entry point instead.


Protocol interaction examples
-----------------------------

### Tool-only (any host)

1. Client initializes or discovers
2. Client calls `tools/call` for `get_time` (e.g., using a "get time" prompt)
3. Server returns text content with an ISO-8601 UTC timestamp

### With MCP Apps (Apps-capable host)

1. Client advertises support for the UI extension as required by the host
2. Server capabilities include `extensions["io.modelcontextprotocol/ui"]`
3. Client lists tools; `get_time` carries `_meta.ui.resourceUri`
4. Host fetches `ui://get-time/mcp-app.html` and renders it in a sandbox
5. View completes `ui/initialize` / `ui/notifications/initialized`
6. Host pushes tool results via `ui/notifications/tool-result`
7. User may press **Get Server Time** (`tools/call` from the iframe)
8. Optional: View sends `ui/message` after a button refresh (host-dependent)


Files
-----

| File                         | Role                                                |
|------------------------------|-----------------------------------------------------|
| `get_time.lgt`               | Application: tool, `ui://` resource, Apps metadata  |
| `mcp-app.html`               | Minimal Apps View (custom `postMessage` client)     |
| `server_2025_06_18.lgt`      | 2025-06-18 stdio server entry point                 |
| `server_2026_07_28.lgt`      | 2026-07-28 stdio server entry point                 |
| `server_streamable_http.lgt` | 2026-07-28 Streamable HTTP server entry point       |
| `loader.lgt`                 | Example loader                                      |
| `tests.lgt`                  | Unit tests (server-side Apps contract)              |
| `tester.lgt`                 | Runs the test suite                                 |
| `NOTES.md`                   | This file                                           |
