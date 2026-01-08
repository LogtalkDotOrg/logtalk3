________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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


`help`
======

This tool provides help for Logtalk features and libraries when running
in most operating-systems. For help on the Logtalk compiler error and
warning messages, see the `tutor` tool.


Requirements
------------

On Windows, the `start` command must be available. On Linux, the `xdg-open`
command must be available. On macOS, the command `open` is used.

Browsing the Handbook and APIs documentation at the top-level requires a
POSIX system and one of the following terminal-based browsers installed:

- https://invisible-island.net/lynx/
- https://w3m.sourceforge.net/
- http://links.twibright.com/
- https://sr.ht/~bptato/chawan/

The preferred browser can be set in a `settings.lgt` file by defining the
`help_default_browser` user-defined flag as follows:

	:- initialization(
		create_logtalk_flag(help_default_browser, Browser, [type(atom), keep(true)])
	).

The valid values for `Browser` are: `lynx`, `w3m`, `links`, `cha`, and
`default` (`default` means use the operating-system default browser
instead of one of the terminal-based browsers).

The `samples/settings-sample.lgt` file contains commented out code for
this flag.

On Windows systems, the documentation is open in the operating-system
default browser.


API documentation
-----------------

This tool API documentation is available at:

[../../apis/library_index.html#help](../../apis/library_index.html#help)


Loading
-------

	| ?- logtalk_load(help(loader)).


Testing
-------

To test this tool, load the `tester.lgt` file:

	| ?- logtalk_load(help(tester)).


Supported operating-systems
---------------------------

Currently, support is limited to Linux, macOS, and Windows.

This tool relies on the library portable operating-system access abstraction.


Usage
-----

After loading the tool, use the query `help::help` to get started:

	| ?- logtalk_load(help(loader)).
	...

	| ?- help::help.
	...

When using ECLiPSe, you will need to write the object name, `help`, between
parentheses to avoid a clash with the "help" built-in operator:

	| ?- (help)::help.
	...

Same sample help queries:

	% get et on-line help for the `logtalk_load/2` built-in predicate:

	| ?- help::logtalk_load/2.
	...

	% get on-line help for the `eos//0` built-in non-terminal:

	| ?- help::eos//0.
	...

	% consult the Handbook:

	| ?- help::handbook.
	...

	% consult the APIs documentation:

	| ?- help::apis.
	...

	% consult the APIs documentation about a library predicate:

	| ?- help::apis(member/2).
	...

	% consult the APIs documentation about a library non-terminal:

	| ?- help::apis(one_or_more//0).
	...

	% consult the documentation of a library:

	| ?- help::library(random).
	...

	% consult the documentation of an entity:

	| ?- help::entity(logtalk).
	...

	% consult the developer tools documentation:

	| ?- help::tools.
	...

	% consult the documentation of the lgtunit tool:

	| ?- help::tool(lgtunit).
	...

When using the terminal-based browsers, after finishing consulting the
documentation and quitting the process, you will be back to the top-level
prompt (if you find that the top-level have scrolled from its last position,
try to set your terminal terminfo to `xterm-256colour`).

If you're running Logtalk from a git clone of its repo, you will need to
run the `docs/apis/sources/build.sh` or `docs/apis/sources/build.ps1`
scripts to generate APIs documentation HTML files and also run the
`docs/handbook/sources/build.sh` or `docs/handbook/sources/build.ps1`
scripts to generate the Handbook HTML files. Alternatively, you can
download the documentation for the latest stable release from the Logtalk
website and save them to the `docs` directories.

On POSIX systems, one of the supported terminal-based browsers must be
installed unless you prefer using the default browser. By default, the
tool checks first for `lynx`, second for `w3m`, third for `links`, and
finally for `chawan`.

On macOS, these browsers can be installed with either MacPorts:

	$ sudo port install lynx
	$ sudo port install w3m
	$ sudo port install links

Or using Homebrew:

	$ brew install lynx
	$ brew install w3m
	$ brew install links
	$ brew install chawan

On Linux systems, use the distribution's own package manager to install the
browsers. For example, in Ubuntu systems:

	$ sudo apt install lynx
	$ sudo apt install w3m


Known issues
------------

When using the terminal-based browsers, the Handbook and APIs search boxes are
not usable as they require JavaScript support. Use instead the indexes.

The open commands used to open documentation URLs in the default browser drop
the fragment part, thus preventing navigation to the specified position on the
documentation page.

ECLiPSe defines a `help` prefix operator that forces wrapping this atom between
parentheses when sending messages to the tool. E.g. use `(help)::help` instead
of `help::help`.
