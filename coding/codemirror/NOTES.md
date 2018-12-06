________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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

In order to use CodeMirror for editing Logtalk source files, copy (or update
if already present) the files `logtalk.js` and `index.html` to a `mode/logtalk`
sub-directory of your CodeMirror installation directory.

The supporting files are work in progress, currently providing syntax highlight
and code folding. The files were obtained by converting the Logtalk Ace mode
using the `ace2cm` converter (and replacing `storage` by `meta` in the token
types in the regular expressions):

	https://github.com/espadrine/ace2cm

The `logtalk.js` and `index.html` files are licensed under the MIT license
as other CodeMirror mode files.

Supported themes must define CSS styles for `meta` and `variable`. Examples
are `ambiance`, `blackboard`, `erlang-dark`, `lesser-dark`, and `mdn-like`.
