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


`debug_messages`
================

By default, `debug` and `debug(Group)` messages are only printed when the `debug`
flag is turned on. These messages are also suppressed when compiling code with the
`optimize` flag turned on. This tool supports selective enabling of `debug` and
`debug(Group)` messages in normal and debug modes.


API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

[docs/library_index.html#debug-messages](https://logtalk.org/docs/library_index.html#debug-messages)

For general information on debugging, open in a web browser the
following file and consult the debugging section of the User Manual:

[manuals/userman/debugging.html](https://logtalk.org/manuals/userman/debugging.html)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(debug_messages(loader)).


Usage
-----

The tool provides two sets of predicates. The first set allows enabling and disabling
of all `debug` and `debug(Group)` messages for a given component. The second set allows
enabling and disabling of `debug(Group)` messages for a given group and component.

Upon loading the tool, all debug messages are skipped. The user is then expected to
use the tool API to selectively enable the messages that will be printed.


Other notes
-----------

All source files are indented using tabs (a common setting is a tab width
equivalent to 4 spaces).
