________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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

This tool provides basic on-line help for Logtalk features and libraries
when running in most operating-systems. For help on the Logtalk compiler
error and warning messages, see the `tutor` tool.


API documentation
-----------------

This tool API documentation is available at:

[../../docs/library_index.html#help](../../docs/library_index.html#help)

For sample queries, please see the `SCRIPT.txt` file in the tool directory.


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

On Windows, the `start` command must be available. On Linux, the `xdg-open`
command must be available. On macOS, the command `open` is used.

This tool relies on the library portable operating-system access abstraction.

Usage
-----

After loading the tool, use the query `help::help` to get started.

Experimental features
---------------------

On POSIX systems, when using Ciao Prolog, ECLiPSe, GNU Prolog (1.5.1 or later
version), LVM, SICStus Prolog, SWI-Prolog, Trealla Prolog, or XSB as the
backend, `apis/1` and `handbook/0-1` predicates are made available. These
predicates open inline at the top-level interpreter the Texinfo versions of
the Handbook and the APIs documentation. The optional argument is a topic
to search, which can be an atom, a predicate indicator, or a non-terminal
indicator. Some examples:

	| ?- help::handbook.

	| ?- help::handbook(base64).

	| ?- help::handbook(logtalk_load/2).

	| ?- help::apis.

	| ?- help::apis(check/2).

	| ?- help::apis(message_tokens//2).

When you finish consult the documentation and quit the `info` process,
you will be back to the top-level prompt (if you find that the top-level
have scrolled from its last position, try to set your terminal terminfo
to `xterm-256colour`).

If you're running Logtalk from a git clone of its repo, you will need to
run the `scripts/update_html_docs.sh` or `scripts/update_html_docs.ps1`
scripts to generate the APIs documentation `.info` file and also run the
`manuals/sources/build_manuals.sh` or `manuals/sources/build_manuals.ps1`
scripts to generated the Handbook `.info` file. In alternative, you can
download the `.info` files for the latest stable release from the Logtalk
website and save them to the `docs` and `manuals` directories.

The required `info` command is provided by the third-party `texinfo` package
(tested with version 6.8). On macOS, this package can be installed with
either MacPorts:

	$ sudo port install texinfo

Or using Homebrew:

	$ brew install texinfo

On Linux systems, use the distribution own package manager to install the
`texinfo` package. For example, in Ubuntu systems:

	$ sudo apt install info

Known issues
------------

The open commands used to open documentation URLs drop the fragment part, thus
preventing navigating to the specified position on the documentation page.

ECLiPSe defines a `help` prefix operator that forces wrapping this atom between
parenthesis when sending messages to the tool. E.g. use `(help)::help` instead
of `help::help`.
