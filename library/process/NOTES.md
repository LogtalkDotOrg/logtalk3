________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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


`process`
=========

This library provides a portable abstraction over process handling predicates
found in some backend Prolog systems. Currently supports ECLiPSe, GNU Prolog,
SICStus Prolog, SWI-Prolog, Trealla Prolog, and XVM.


API documentation
-----------------

Open the [../../docs/library_index.html#process](../../docs/library_index.html#process)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(process(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(process(tester)).


Usage
-----

The `process` object provides the following predicates for portable process handling:

- `create(Executable, Arguments, Options)` - Creates a new process
- `wait(Process, Status)` - Waits for a process to terminate
- `kill(Process, Signal)` - Terminates a process with a specific signal
- `kill(Process)` - Terminates a process using the default signal (SIGKILL)

The `create/3` predicate supports the following options:

- `process(Pid)` - Unifies Pid with the process identifier (an opaque ground term)
- `stdin(Stream)` - Binds Stream to the process standard input
- `stdout(Stream)` - Binds Stream to the process standard output
- `stderr(Stream)` - Binds Stream to the process standard error

Note: Process identifiers (PIDs) should be treated as opaque ground terms.
Their internal representation varies between backend Prolog systems.

Example:

    | ?- process::create('/bin/echo', ['hello.'], [stdout(Out), process(PID)]),
         read_term(Out, Term, []),
         close(Out).
    Out = ..., PID = ..., Term = hello.
