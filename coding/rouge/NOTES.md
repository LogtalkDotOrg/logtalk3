________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains the `logtalk.rb` file that provides syntax 
coloring support for Rouge (version 1.9.1 or later):

	http://rouge.jneen.net/

These support files are dual-licensed under the Apache License 2.0 and the
Rouge license.

Rouge may already include support for Logtalk. You can check it by running
the command:

	$ rougify list

This directory may contain, however, updated support files. If that is the
case, install the Logtalk support files by performing the following steps:

1. Copy the file `lib/rouge/lexers/logtalk.rb` to the same directory in 
your Rouge installation directory (replacing any existing older file).

2. Copy the file `lib/rouge/demos/logtalk` to the same directory in 
your Rouge installation directory (replacing any existing older file).

3. Copy the file `spec/lexers/logtalk_spec.rb` to the same directory in 
your Rouge installation directory (if it exists, replacing any existing
older file).

4. Copy the file `spec/visual/samples/logtalk` to the same directory in 
your Rouge installation directory (if it exists, replacing any existing
older file).

An example of generating HTML code (wrapped with a `pre` tag) would be:

	$ rougify highlight -f html -i source.lgt > source.html

To get the CSS file used for the generated HTML code use the command:

	$ rougify style > source.css

An useful formatter option is line numbers:

	$ rougify highlight -f html -F line_numbers=true -i source.lgt > source.html

You may also use the Logtalk lexer with plain Prolog files by using the `-l` 
option. For example:

	$ rougify highlight -l logtalk -i source.pl > source.html
