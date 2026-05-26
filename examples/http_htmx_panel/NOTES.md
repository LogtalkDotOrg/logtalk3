---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the License);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an AS IS BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

# http_htmx_panel

This example shows a small HTTP server and HTTP client using the
`http_htmx` library together with the companion `http_router_htmx`
middleware category.

This example illustrates six basic steps:

1. Import `http_router` and `http_router_htmx` in one handler object.
2. Use `http_htmx::reply/3` for an ordinary HTML page route.
3. Use `http_htmx::page_fragment_reply/4` for panel routes that return a full
  page for ordinary and boosted navigation and only a fragment for
  non-boosted HTMX requests.
4. Use `hx-get`, `hx-target`, `hx-indicator`, `hx-swap`, and `hx-push-url`
  on the fragment buttons to make the studio visibly interactive.
5. Let the router middleware derive HTMX request properties and add
  `HX-Trigger` response headers for HTMX requests.
6. Refresh a visible activity rail by listening for those `HX-Trigger`
  events with hidden HTMX relays instead of inline JavaScript.

Load the example with:

```logtalk
logtalk_load(http_htmx_panel(loader)).
```

When backend threads are available, run the complete self-contained demo:

```logtalk
http_htmx_panel_demo::run(Result).
```

The returned `Result` term contains four normalized responses:

1. the ordinary home page served from `/`
2. the ordinary page response served from `/panel`
3. the non-boosted HTMX fragment response served from `/panel`
4. the boosted HTMX page response served from `/panel`

The direct client can also be used against an already running server:

```logtalk
htmx_panel_client::run(8080, Result).
```

When backend threads are not available, run the server and client in separate
sessions. In the first session start the server:

```logtalk
htmx_panel_server::serve(8080, 4).
```

Then, in a second session, fetch the same four resources:

```logtalk
htmx_panel_client::fetch_home(8080, HomeResponse).
htmx_panel_client::fetch_panel_page(8080, PanelPageResponse).
htmx_panel_client::fetch_panel_fragment(8080, PanelFragmentResponse).
htmx_panel_client::fetch_panel_boosted(8080, PanelBoostedResponse).
```

To explore the example with a web browser, start the server with a larger
connection count:

```logtalk
htmx_panel_server::serve(8080, 20).
```

Then open `http://127.0.0.1:8080/` in a browser. The home page contains three
visible areas:

- a navigation card with ordinary and boosted links to `/panel`
- a studio card with three fragment buttons for `/panel/overview`,
  `/panel/metrics`, and `/panel/alert`
- an activity rail refreshed through hidden HTMX relays listening for
  `HX-Trigger` events from the panel routes

The page also loads the HTMX browser runtime from a CDN so those HTMX
controls work when you open the example directly in a browser.

Those controls exercise both full-page navigation and in-place fragment swaps.
The fragment buttons also show a visible loading pill while a request is in
flight and push the selected panel URL into browser history.

The panel routes demonstrate route metadata plus response middleware by
adding `HX-Trigger` response headers. The page includes hidden HTMX relay
elements listening for those custom events from `body`, and those relays fetch
small server-rendered fragments into the activity rail.

Study the [http_htmx_panel.lgt](http_htmx_panel.lgt) source file together with
these sample queries. The example is intentionally small so the `http_htmx`
reply helpers, the `http_router_htmx` middleware, the ordinary-vs-fragment
behavior, and the server-triggered UI refreshes stay visible at the same time.
