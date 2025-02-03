---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.1'
      jupytext_version: 1.16.6
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
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
-->

# profiling

This is a very simple example of the use of events and monitors to make 
profilers for an application. It's easy to modify to make it do much more. 
For instance, most Prolog compilers give you access to data concerning
space usage (stacks, heap, etc).

The example defines three objects:

- `message_counter`  
	using events, this object allows us to count the messages sent to
	spied objects 

- `stop_watch`  
	using events, this object simply prints the CPU time before and
	after a message sent to a spied object

- `timer`  
	this object implements a method that sends a message to an object
	a specified number of times, returning the average execution time

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(profiling(loader)).
```

Turn event support on for messages sent at the command-line
(i.e., for messages sent from the pseudo-object _user_):

```logtalk
set_logtalk_flag(events, allow).
```

<!--
true.
-->

### `message_counter` example:


Choose an object to spy:

```logtalk
message_counter::set_spy_point(_, list, _, _).
```

<!--
true.
-->

Activate the monitor:

```logtalk
message_counter::activate_monitor.
```

<!--
true.
-->

Send some messages to the spied object; get all the answers for `ancestor/1`:

```logtalk
list::empty([]).
```

<!--
true.
-->

```logtalk
list::member(X, [1, 2, 3]).
```

<!--
X = 1 ? ;
X = 2 ? ;
X = 3 ? ;
false.
-->

Print a report of the data collected by the monitor:

```logtalk
message_counter::report.
```

<!--
list
  total of calls: 2
  total of exits: 4

  empty/1
    calls: 1
    exits: 1

  member/2
    calls: 1
    exits: 3

true.
-->

Stop and reset the message counter monitor:

```logtalk
message_counter::stop.
```

<!--
true.
-->

### `stop_watch` example


Choose a pair object/message to spy:

```logtalk
stop_watch::set_spy_point(_, list, length(_, _), _).
```

<!--
true.
-->

Activate the monitor:

```logtalk
stop_watch::activate_monitor.
```

<!--
true.
-->

Send a message to the spied object:

```logtalk
list::length([1, 2, 3], Length).
```

<!--
list <-- length([1,2,3],_277) from user
STARTING at 755.69000005 seconds
list <-- length([1,2,3],3) from user
ENDING at 755.69000235 seconds

Length = 3.
-->

Clean up:

```logtalk
stop_watch::reset_monitor.
```

<!--
true.
-->

### `timer` example


Try a message 1000 times and return the average time:

```logtalk
timer::timer(list::length([1, 2, 3], _), 1000, Time).
```

<!--
Time = 0.00003
-->

Turn event support off:

```logtalk
set_logtalk_flag(events, deny).
```

<!--
true.
-->
