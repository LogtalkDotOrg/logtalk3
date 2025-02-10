---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
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

# threads - philosophers

This folder contains a Logtalk implementation of the classical "dining
philosophers" multi-threading problem.

For more information, consult e.g. the following URL:

http://en.wikipedia.org/wiki/Dining_philosophers_problem

Two different implementations are provided, both using the same solution for
avoiding deadlock (which is having one philosopher picking its chopsticks
in a different order from the other philosophers; see the URL above for
details): one implementation uses a category and five philosopher objects
while the second implementation uses a parametric object.

This example should preferably be run from the top-level instead of as
a notebook to observe the threads output.

When using XVM as the backend, this example must be run from the top-level
interpreter.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(philosophers(loader)).
```

Five meals for each philosopher, each one spending a maximum of five seconds of continuous thinking or eating:

```logtalk
threaded_ignore(p1::run(5, 5)), threaded_ignore(p2::run(5, 5)), threaded_ignore(p3::run(5, 5)), threaded_ignore(p4::run(5, 5)), threaded_ignore(p5::run(5, 5)).
```

<!--
true.

Philosopher p3 thinking for 1 seconds.
Philosopher p1 thinking for 2 seconds.
Philosopher p2 thinking for 3 seconds.
Philosopher p4 thinking for 4 seconds.
Philosopher p5 thinking for 3 seconds.
Philosopher p3 eating for 2 seconds with chopsticks cs3 and cs2.
Philosopher p1 eating for 3 seconds with chopsticks cs5 and cs1.
Philosopher p2 thinking for 3 seconds.
Philosopher p3 thinking for 2 seconds.
Philosopher p5 thinking for 1 seconds.
Philosopher p4 eating for 1 seconds with chopsticks cs4 and cs3.
Philosopher p5 thinking for 1 seconds.
Philosopher p1 thinking for 3 seconds.
Philosopher p4 thinking for 1 seconds.
Philosopher p3 eating for 2 seconds with chopsticks cs3 and cs2.
Philosopher p5 eating for 2 seconds with chopsticks cs5 and cs4.
Philosopher p2 thinking for 3 seconds.
Philosopher p4 thinking for 2 seconds.
Philosopher p3 thinking for 2 seconds.
Philosopher p5 thinking for 1 seconds.
Philosopher p1 eating for 3 seconds with chopsticks cs5 and cs1.
Philosopher p4 eating for 4 seconds with chopsticks cs4 and cs3.
Philosopher p5 thinking for 1 seconds.
Philosopher p2 thinking for 1 seconds.
Philosopher p3 thinking for 4 seconds.
Philosopher p5 thinking for 2 seconds.
Philosopher p2 thinking for 4 seconds.
Philosopher p1 thinking for 1 seconds.
Philosopher p5 thinking for 1 seconds.
Philosopher p4 thinking for 4 seconds.
Philosopher p1 eating for 1 seconds with chopsticks cs5 and cs1.
Philosopher p5 thinking for 3 seconds.
Philosopher p3 eating for 2 seconds with chopsticks cs3 and cs2.
Philosopher p1 thinking for 3 seconds.
Philosopher p2 thinking for 4 seconds.
Philosopher p3 thinking for 3 seconds.
Philosopher p5 eating for 4 seconds with chopsticks cs5 and cs4.
Philosopher p4 thinking for 3 seconds.
Philosopher p1 thinking for 4 seconds.
Philosopher p2 eating for 1 seconds with chopsticks cs1 and cs2.
Philosopher p3 thinking for 3 seconds.
Philosopher p4 thinking for 3 seconds.
Philosopher p2 thinking for 1 seconds.
Philosopher p5 thinking for 2 seconds.
Philosopher p2 eating for 3 seconds with chopsticks cs1 and cs2.
Philosopher p1 thinking for 4 seconds.
Philosopher p3 thinking for 3 seconds.
Philosopher p5 eating for 2 seconds with chopsticks cs5 and cs4.
Philosopher p4 thinking for 1 seconds.
Philosopher p4 thinking for 4 seconds.
Philosopher p2 thinking for 1 seconds.
Philosopher p5 thinking for 4 seconds.
Philosopher p1 eating for 4 seconds with chopsticks cs5 and cs1.
Philosopher p2 thinking for 2 seconds.
Philosopher p3 eating for 2 seconds with chopsticks cs3 and cs2.
Philosopher p2 thinking for 3 seconds.
Philosopher p3 thinking for 1 seconds.
Philosopher p4 eating for 1 seconds with chopsticks cs4 and cs3.
Philosopher p5 thinking for 1 seconds.
Philosopher p3 thinking for 4 seconds.
Philosopher p4 thinking for 4 seconds.
Philosopher p1 thinking for 2 seconds.
Philosopher p5 eating for 2 seconds with chopsticks cs5 and cs4.
Philosopher p2 eating for 2 seconds with chopsticks cs1 and cs2.
Philosopher p1 thinking for 3 seconds.
Philosopher p5 thinking for 4 seconds.
Philosopher p2 thinking for 2 seconds.
Philosopher p3 eating for 4 seconds with chopsticks cs3 and cs2.
Philosopher p4 thinking for 2 seconds.
Philosopher p1 eating for 3 seconds with chopsticks cs5 and cs1.
Philosopher p2 thinking for 1 seconds.
Philosopher p4 thinking for 3 seconds.
Philosopher p2 thinking for 1 seconds.
Philosopher p5 thinking for 3 seconds.
p3 terminated.
Philosopher p2 thinking for 3 seconds.
p1 terminated.
Philosopher p4 eating for 3 seconds with chopsticks cs4 and cs3.
Philosopher p5 thinking for 4 seconds.
Philosopher p2 eating for 4 seconds with chopsticks cs1 and cs2.
Philosopher p4 thinking for 4 seconds.
Philosopher p5 eating for 1 seconds with chopsticks cs5 and cs4.
Philosopher p2 thinking for 1 seconds.
p5 terminated.
Philosopher p2 eating for 4 seconds with chopsticks cs1 and cs2.
Philosopher p4 eating for 3 seconds with chopsticks cs4 and cs3.
p4 terminated.
p2 terminated.
-->

Same problem as above but using a parametric object for representing the philosophers:

```logtalk
threaded_ignore(philosopher(p1,cs1,cs2)::run(5, 5)), threaded_ignore(philosopher(p2,cs2,cs3)::run(5, 5)), threaded_ignore(philosopher(p3,cs3,cs4)::run(5, 5)), threaded_ignore(philosopher(p4,cs4,cs5)::run(5, 5)), threaded_ignore(philosopher(p5,cs1,cs5)::run(5, 5)).
```
