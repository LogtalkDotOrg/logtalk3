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
SICStus Prolog, Shumway Prolog, SWI-Prolog, Trealla Prolog, and XVM.


API documentation
-----------------

Open the [../../apis/library_index.html#process](../../apis/library_index.html#process)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

    | ?- logtalk_load(process(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

    | ?- logtalk_load(process(tester)).


Usage
-----

The `process` object provides the following predicates for portable process
handling:

- `create(Executable, Arguments, Options)` - Creates a new process
- `wait(Process, Status)` - Waits for a process to terminate
- `wait(Process, RawStatus, NormalizedStatus)` - Waits once and returns both raw and normalized status
- `kill(Process, Signal)` - Terminates a process with a specific signal
- `kill(Process)` - Terminates a process using the default signal (SIGKILL)

The `wait/2` predicate returns the backend's native status unchanged. For a
successfully terminated process, `Status` is either `0` or `exit(0)`.

The `wait/3` predicate reaps the process once and returns the backend-specific
result in `RawStatus` and its normalized form in `NormalizedStatus`. The latter
is `exit(Code)`, `killed(Signal)`, or `unknown(RawStatus)`.
Nonzero application exit codes are returned as results when the native API
reports them normally. Native waiting errors and failures are propagated.
The normalized status does not separately report core-dump information.

On POSIX, ECLiPSe returns encoded wait words; on Windows it returns direct exit
codes. GNU Prolog's POSIX API converts normal exits to integers but retains
encoded signal statuses, making nonzero results ambiguous. These results are
returned as `unknown(RawStatus)`, not guessed to be normal exits or signals. GNU
Prolog also reports normal exit codes 126 and 127 as native errors; `wait/3`
does not hide this limitation. Structured native `exit/1` and `killed/1` statuses
are preserved. Other unrecognized native statuses remain `unknown(RawStatus)`.
XVM's native `signal(Signal)` status is normalized to `killed(Signal)`.

The `create/3` predicate supports the following options:

- `process(Pid)` - Unifies Pid with the process identifier (an opaque ground term)
- `stdin(Stream)` - Binds Stream to the process standard input
- `stdout(Stream)` - Binds Stream to the process standard output
- `stderr(Stream)` - Binds Stream to the process standard error
- `type(Type)` - Sets the type (`binary` or `text`) of the `stdin/1` and `stdout/1` streams

Process identifiers (PIDs) should be treated as opaque ground terms. Their
internal representation varies between backend Prolog systems.

Close requested pipes and wait for every process returned through `process/1`,
including after calling `kill/1` or `kill/2`. Drain a child's output while it is
running, before waiting for termination. Waiting first can deadlock on a full
pipe. Sequentially draining independent, unbounded stdout and stderr pipes can
also deadlock.

The `type/1` option is ignored on ECLiPSe, GNU Prolog, Trealla Prolog, and XVM
as these systems don't provide the necessary support to set the streams type
at process creation.

Example:

    | ?- process::create('/bin/echo', ['hello.'], [stdout(Out), process(PID)]),
         read_term(Out, Term, []),
         close(Out),
            process::wait(PID, RawStatus, NormalizedStatus).
        Out = ..., PID = ..., Term = hello, RawStatus = ..., NormalizedStatus = exit(0).
