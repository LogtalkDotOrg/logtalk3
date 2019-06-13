________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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
This tool provides basic on-line help for Logtalk when running in a limited
set of operating-systems.


API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

[docs/library_index.html#help](https://logtalk.org/docs/library_index.html#help)

For sample queries, please see the [SCRIPT.txt](SCRIPT.txt) file.


Loading
-------

	| ?- logtalk_load(help(loader)).


Supported operating-systems
---------------------------

Currently, support is limited to Linux, macOS, and Windows.

On Windows, the `start` command must be available. On Linux, the `xdg-open`
command must be available. On macOS, the command `open` is used.

This tool relies on the library portable operating-system access abstraction.


Known issues
------------

On macOS, the `open` command used to open documentation URLs drops the anchors,
thus preventing navigating to the specified position on the documentation page.

ECLiPSe defines a `help` prefix operator that forces wrapping this atom between
parenthesis when sending messages to the tool. E.g. use `(help)::help` instead
of `help::help`.


Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
