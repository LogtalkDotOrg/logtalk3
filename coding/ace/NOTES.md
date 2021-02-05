________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


Ace is an embeddable code editor written in JavaScript available from:

	https://ace.c9.io/

In order to use Ace for editing Logtalk source files, copy the files
`logtalk.js` and `logtalk_highlight_rules.js` to the `lib/ace/mode`
sub-directory of your Ace installation directory.

The supporting files are work in progress and were obtained by first
automatically converting the `Logtalk.tmLanguage` TextMate file using
Ace's TMLanguage Tool and fixing some of the resulting conversion issues.

The `logtalk.js` and `logtalk_highlight_rules.js` files are licensed under
the BSD license as other Ace mode files.
