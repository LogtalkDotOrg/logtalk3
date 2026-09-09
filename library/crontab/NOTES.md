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


`crontab`
=========

Parser and generator for user and system crontab files. The library supports
classic five-field schedules, environment assignments, comments, blank lines,
system crontab users, named months and weekdays, standard Vixie/Cronie
nicknames, and percent-separated command input.


API documentation
-----------------

Open the [../../apis/library_index.html#crontab](../../apis/library_index.html#crontab)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(crontab(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(crontab(tester)).


Text representations
--------------------

The `crontab(Representation)` parametric object accepts `atom`, `chars`, and
`codes`. The parameter controls free textual values in parsed entries. The
non-parametric `crontab` object defaults to atoms.

For example, parsing the same comment produces `comment(' jobs')`,
`comment([' ',j,o,b,s])`, or `comment([32,106,111,98,115])`. Schedule names
such as `jan`, `mon`, and `daily` are semantic atoms in all representations.

Input and output transport is independent of the term representation. Sources
and sinks can be `file(Path)`, `stream(Stream)`, `atom(Atom)`, `chars(Chars)`,
or `codes(Codes)`. Streams supplied by callers remain open. Streams opened for
`file(Path)` are closed by the library.


Terms
-----

The parser returns a list containing these terms:

- `comment(Text)`
- `blank`
- `env(Name, Value)`
- `entry(Schedule, Command, Input)` for user crontabs
- `user_entry(User, Schedule, Command, Input)` for system crontabs

A schedule is either `time(Minute, Hour, DayOfMonth, Month, DayOfWeek)` or
`special(Name)`. Fields use `*`, an integer or symbolic name, `range(Low,High)`,
`step(Base,Step)`, or `list(Items)`. Input is `none` or `stdin(Text)`.


Parsing and generation
----------------------

The default format is a user crontab:

    | ?- crontab::parse(atom('0 2 * * * backup\n'), Entries).
    Entries = [entry(time(0,2,*,*,*),backup,none)].

Use the `format(system)` option for system crontabs:

    | ?- crontab::parse(atom('0 2 * * * root backup\n'), Entries, [format(system)]).
    Entries = [user_entry(root,time(0,2,*,*,*),backup,none)].

Generation uses the same option and emits canonical spacing and LF line
endings. Aliases `@annually` and `@midnight` are normalized to `@yearly` and
`@daily`. Unescaped percent signs split a command from its standard input;
subsequent percent signs represent line feeds.
