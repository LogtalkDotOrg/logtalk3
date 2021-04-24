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


`bat` is a `cat` clone that supports syntax highlighting and `git`.
It's available from:

	https://github.com/sharkdp/bat

`bat` supports Sublime Text `.sublime-syntax` files. To configure it
for Logtalk support see the instructions on its website and use the
`../sublimetext/Logtalk.sublime-syntax` file.

To also use the Logtalk support for syntax coloring of Prolog files,
either create an alias to `bat` that calls it with the `-l logtalk`
option (e.g. for a Bash shell: `alias batpl='bat -l logtalk'`) or
edit the `bat` configuration file to map Prolog extensions to use
the Logtalk syntax highlighter. For example, using `nano` as the
text editor:

	$ nano "$(bat --config-file)"

and add one line per Prolog file extension. For example:

	--map-syntax pl:logtalk
	--map-syntax pro:logtalk
