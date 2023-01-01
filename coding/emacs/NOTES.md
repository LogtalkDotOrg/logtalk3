________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains a `logtalk.el` file that provides syntax
highlighting for editing Logtalk source files with the Emacs text
editor:

	http://www.gnu.org/software/emacs/emacs.html

These support files are dual-licensed under the Apache License 2.0 and the
Emacs license.

To install logtalk-mode follow the instructions contained in the `logtalk.el` file itself.

Emacs regular expressions don't support look-ahead assertions, which
result is syntax coloring errors in valid code such as the `0'"` term.

Also included are a set of snippets for use with
[yasnippet](https://github.com/joaotavora/yasnippet) and logtalk-mode. To
install these snippets move the `logtalk-mode` directory into `yas-snippet-dirs`
(by default `~/.emacs.d/snippets`).  To set the `author` for the snippets, in
your `init.el` add:

```
(setq-default logtalk-snippets-author "Your Name")
```

Included snippets:
- `obj` (object)
- `cat` (category)
- `pro` (protocol)
- `public` (public declaration)
- `protected` (protected declaration)
- `private` (private declaration)
- `loader` (loader file template)
- `tester` (tester file template)
- `tests` (tests template)
- and more, see the `key` in the file headings for the triggers.
