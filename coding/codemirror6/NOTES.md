________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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


CodeMirror is an embeddable code editor written in JavaScript available from:

	https://codemirror.net/

The files in this directory provide support for CodeMirror 6.x.

In order to use CodeMirror for editing Logtalk source files, copy (or update
if already present) the files `logtalk.js` and `index.html` to a `mode/logtalk`
sub-directory of your CodeMirror installation directory. The `logtalk.js` file
implements the mode while the `index.html` shows a usage example.

The supporting files are work in progress, currently providing syntax
highlighting.

The `logtalk.js` and `index.html` files are licensed under the MIT license
as other CodeMirror mode files.

Supported themes must define CSS styles for `meta` and `variable`. Examples
are `ambiance`, `blackboard`, `erlang-dark`, `lesser-dark`, and `mdn-like`.

Supported Language Features
---------------------------

- Syntax highlighting for all Logtalk constructs
- Syntax highlighting for all standard Prolog constructs

Basic Usage
-----------

```javascript
import { EditorView, basicSetup } from "codemirror";
import { logtalk } from "./logtalk.js";

const view = new EditorView({
    doc: "% Your Logtalk code here",
    extensions: [
        basicSetup,
        logtalk
    ],
    parent: document.getElementById('editor')
});
```

Custom Usage
------------

```javascript
import { EditorView } from "@codemirror/view";
import { EditorState } from "@codemirror/state";
import { syntaxHighlighting, defaultHighlightStyle } from "@codemirror/language";
import { logtalk } from "./logtalk.js";

const state = EditorState.create({
    doc: "% Your Logtalk code here",
    extensions: [
        logtalk,
        syntaxHighlighting(defaultHighlightStyle),
        // Add other extensions as needed
    ]
});

const view = new EditorView({
    state,
    parent: document.getElementById('editor')
});
```

Dependencies
------------

- `@codemirror/language` - For the StreamLanguage interface
- `@codemirror/view` - For the editor view (if using the basic example)
- `@codemirror/state` - For editor state management

Installation
------------

1. Install CodeMirror 6 packages:

```bash
npm install @codemirror/language @codemirror/view @codemirror/state
```

2. Import the Logtalk language support:

```javascript
import { logtalk } from "./logtalk.js";
```

3. Add it to your editor configuration as shown in the usage examples above.

Examples
--------

See `index.html` for a complete working example that demonstrates the Logtalk
language support in action.

Browser Compatibility
---------------------

This implementation works with all modern browsers that support ES6 modules.
For older browser support, you'll need to use a bundler like Webpack, Rollup,
or Vite.
