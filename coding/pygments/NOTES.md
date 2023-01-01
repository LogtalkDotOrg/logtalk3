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


This directory contains the `logtalk.py` file that provides syntax 
coloring support for Pygments (version 0.9 or later):

	http://pygments.org/

These support files are dual-licensed under the Apache License 2.0 and the
Pygments license.

Pygments includes support for Logtalk since version 0.10 as you can check using
the command:

	$ pygmentize -H lexer Logtalk

If support for Logtalk is outdated, replace its definition inside one of the
following files (its prefix depends on where Python is installed):

	site-packages/pygments/lexers/prolog.py (recent Pygments versions)
	site-packages/pygments/lexers/other.py  (older Pygments versions)

with the contents of the `logtalk.py` from the Logtalk distribution and compile
the updated file:

	$ python -m py_compile prolog.py (recent Pygments versions)
	$ python -m py_compile other.py  (older Pygments versions)

Finally, rebuild the lexer mappings by typing:

	$ python _mapping.py

Logtalk source files (including the library entities and the programming
examples) are indented using tabs (a common setting is a tab width
equivalent to 4 spaces). This can be specified by using the `whitespace`
filter option `tabsize`:

	$ pygmentize -F whitespace:tabsize=4 -O full,style=friendly -o source.html source.lgt

When your source file uses an encoding other than ASCII or ISO-Latin-1, 
you will need to use the `encoding` option. For example:

	$ pygmentize -F whitespace:tabsize=4 -O encoding=utf8 -o babel.html babel.lgt

As the Pygments syntax highlighting engine itself, the `logtalk.py` file 
is licensed under the BSD license.

You may also use the Logtalk lexer with plain Prolog files by using the `-l` 
option. For example:

	$ pygmentize -l logtalk -O full,style=friendly -o source.html source.pl
