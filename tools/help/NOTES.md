________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>

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


Overview
--------

This tool provides basic on-line help for Logtalk when running in a limited
set of operating-systems. 


API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

[docs/directory_index.html#tools/help/](http://logtalk.org/docs/directory_index.html#tools/help/)

For sample queries, please see the `SCRIPT.txt` file.


Loading
-------

	| ?- logtalk_load(help(loader)).


Supported operating-systems
---------------------------

Currently, support is limited to Linux, MacOS X, and Windows.

On Windows, the `start` command must be available. On Linux, the `xdg-open`
command must be available. On MacOS X, the command `open` is used.

This tool relies on the library portable operating-system access abstraction.


Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
