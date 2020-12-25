________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

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


This folder contains a single Prolog file, `core.pl`, which implements the
Logtalk compiler and runtime. There are also several Logtalk source files
defining built-in protocols, categories, and objects:

- `expanding.lgt`  
	built-in `expanding` protocol specifying term- and goal-expansion predicates
- `forwarding.lgt`  
	built-in `forwarding` protocol specifying the message forwarding predicate
- `monitoring.lgt`  
	built-in `monitoring` protocol specifying the event handler predicates
- `logtalk.lgt`  
	built-in `logtalk` object defining message printing, question asking, debugging, and hacking predicates
- `core_messages.lgt`  
	built-in `core_messages` category defining the default translations for compiler messages
- `user.lgt`  
	definition of the built-in pseudo-object `user`

Before loading the `core.pl` file into your favorite Prolog compiler,
you must first load the appropriated adapter file for your Prolog
compiler, which you will find in the `adapters` directory, and the
`paths/paths.pl` file, which defines essential library paths for
starting Logtalk. The provided Prolog POSIX integration scripts and
Windows shortcuts automate this process and should be used unless
there's a strong reason to manually load Logtalk.

HTML documentation for the core entity APIs can be found on the `docs`
directory (open the `docs/index.html` file with your web browser). The
documentation for these tools can be regenerated using the shell scripts
`../scripts/update_html_docs.sh` and `../scripts/update_svg_diagrams.sh`.

The source files are indented using tabs (a common setting is a tab
width equivalent to 4 spaces).
