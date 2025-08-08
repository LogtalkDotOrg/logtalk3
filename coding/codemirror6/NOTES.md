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

The files in this directory provide comprehensive support for CodeMirror 6.x using a
Lezer-based parser for enhanced syntax highlighting and automatic indentation.

In order to use CodeMirror for editing Logtalk source files, copy (or update
if already present) the files `logtalk.js`, `src/logtalk-lezer.js`, `src/parser.js`,
`src/logtalk.grammar`, and `index.html` to your project directory. The `logtalk.js`
file exports the language support while the `index.html` shows a comprehensive usage example.

The implementation uses a Lezer parser generated from a complete Logtalk grammar,
providing accurate syntax highlighting and context-aware automatic indentation.

The `logtalk.js` and related files are licensed under the MIT license
as other CodeMirror mode files.

Supported Language Features
---------------------------

- **Complete syntax highlighting** for all Logtalk constructs using Lezer parser
- **Context-aware automatic indentation** with proper handling of:
  - Entity definitions (objects, protocols, categories, modules)
  - Clause definitions and bodies
  - Nested constructs (compound terms, lists, control structures)
  - Directives and entity relations
- **Syntax highlighting** for all standard Prolog constructs
- **Code folding** support for entities and complex structures
- **Bracket matching** and auto-closing
- **Error-tolerant parsing** that continues highlighting even with syntax errors
- **Performance optimized** incremental parsing for large files

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

- `@codemirror/language` - For LRLanguage and indentation support
- `@codemirror/view` - For the editor view
- `@codemirror/state` - For editor state management
- `@lezer/highlight` - For syntax highlighting with style tags
- `@lezer/lr` - For the Lezer parser runtime
- `@lezer/generator` - For building the parser from grammar (dev dependency)

Installation
------------

1. Install CodeMirror 6 packages and Lezer dependencies:

```bash
npm install @codemirror/language @codemirror/view @codemirror/state @lezer/highlight
npm install --save-dev @lezer/generator
```

2. Build the Lezer parser (if modifying the grammar):

```bash
npm run build-parser
```

3. Import the Logtalk language support:

```javascript
import { logtalk } from "./logtalk.js";
```

4. Add it to your editor configuration as shown in the usage examples above.

Implementation Details
---------------------

This implementation uses a **Lezer parser** generated from a comprehensive Logtalk
grammar (`src/logtalk.grammar`). The Lezer parser provides several advantages over
the previous stream-based parser:

- **Accurate parsing**: Full syntax tree construction enables precise syntax highlighting
- **Context awareness**: Indentation and highlighting based on syntactic context
- **Error recovery**: Continues parsing and highlighting even with syntax errors
- **Performance**: Incremental parsing for efficient updates in large files
- **Extensibility**: Easy to extend with new language features

The parser recognizes all major Logtalk constructs including:
- Entity definitions (objects, protocols, categories, modules)
- Directives and entity relations
- Clause definitions with heads and bodies
- All operators and built-in predicates
- Comments, strings, numbers, and variables
- Control structures and message passing

Examples
--------

See `index.html` for a complete working example that demonstrates the Logtalk
language support in action with comprehensive test cases.

Browser Compatibility
---------------------

This implementation works with all modern browsers that support ES6 modules.
For older browser support, you'll need to use a bundler like Webpack, Rollup,
or Vite.
