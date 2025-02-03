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

# bricks

This folder contains an example of representation and handling of relations
using events. We have instances of class `brick` and a binary `brick_stack`
relation between the bricks. Every time we move a brick, we want the bricks
on top of it to move along. If we break the stack by moving a middle brick,
we want to automatically destroy the corresponding relation tuple.

It's instructive to use the debugger to better understand this example.
Set spy points in all block instances and then activate the debugger.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(bricks(loader)).
```

Create four bricks, all standing on the "ground" (use your imagination... ;-)
(don't use message broadcasting syntax in order to workaround a XSB parser bug):

```logtalk
brick::new(a, [position-(8, 1)]), brick::new(b, [position-(6, 1)]), brick::new(c, [position-(4, 1)]), brick::new(d, [position-(2, 1)]).
```

<!--
true.
-->

Set up ASCII stack monitor so we can watch the bricks moving:

```logtalk
after_event_registry::set_monitor(_, move(_,_), _, stack_monitor).
```

<!--
true.
-->

Ensure that top-level message goals generate events:

```logtalk
set_logtalk_flag(events, allow).
```

<!--
true.
-->

Make the stack (don't use message broadcasting syntax in order to workaround a XSB parser bug):

```logtalk
brick_stack::add_tuple([c,d]), brick_stack::add_tuple([b,c]), brick_stack::add_tuple([a,b]).
```

<!--
|.c......
|.d...b.a
---------
|.b......
|.c......
|.d.....a
---------
|.a
|.b
|.c
|.d
---
true.
-->

List all tuples:

```logtalk
forall(brick_stack::tuple(Tuple), (write(Tuple), nl)).
```

<!--
[c,d]
[b,c]
[a,b]

true.
-->

List all _before_ and _after_ monitors:

```logtalk
before_event_registry::monitors(BeforeMonitors), after_event_registry::monitors(AfterMonitors).
```

<!--
AfterMonitors = [brick_stack, stack_monitor], BeforeMonitors = [brick_stack].
-->

Move all stack to new position by moving bottom brick; check results:

```logtalk
d::move(9, 1).
```

<!--
|.a.......
|.b.......
|.c.......
|........d
----------
|.a.......
|.b.......
|........c
|........d
----------
|.a.......
|........b
|........c
|........d
----------
|........a
|........b
|........c
|........d
----------
true.
-->

Check results:

```logtalk
a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).
```

<!--
Xa = 9, Xb = 9, Xc = 9, Xd = 9, Ya = 4, Yb = 3, Yc = 2, Yd = 1.
-->

List all tuples:

```logtalk
forall(brick_stack::tuple(Tuple), (write(Tuple), nl)).
```

<!--
[c,d]
[b,c]
[a,b]

true.
-->

Break the stack in half by moving `b` to the "ground":

```logtalk
b::move(3, 1).
```

<!--
|........a
|.........
|........c
|..b.....d
----------
|..a.....c
|..b.....d
----------
true.
-->

Check results:

```logtalk
a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).
```

<!--
Xa = 3, Xb = 3, Xc = 9, Xd = 9, Ya = 2, Yb = 1, Yc = 2, Yd = 1.
-->

List all tuples:

```logtalk
forall(brick_stack::tuple(Tuple), (write(Tuple), nl)).
```

<!--
[c,d]
[a,b]

true.
-->

Create new `brick_stack` tuple and check results:

```logtalk
brick_stack::add_tuple([d, a]).
```

<!--
|..d......
|..a.....c
|..b......
----------
|..c
|..d
|..a
|..b
----
true.
-->

Check results:

```logtalk
a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).
```

<!--
Xa = 3, Xb = 3, Xc = 3, Xd = 3, Ya = 2, Yb = 1, Yc = 4, Yd = 3.
-->

List all tuples:

```logtalk
forall(brick_stack::tuple(Tuple), (write(Tuple), nl)).
```

<!--
[c,d]
[a,b]
[d,a]

true.
-->

Move the stack to new position by moving bottom brick; check results:

```logtalk
b::move(5, 1).
```

<!--
|..c..
|..d..
|..a..
|....b
------
|..c..
|..d..
|....a
|....b
------
|..c..
|....d
|....a
|....b
------
|....c
|....d
|....a
|....b
------
true.
-->

Check results:

```logtalk
a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd).
```

<!--
Xa = 5, Xb = 5, Xc = 5, Xd = 5, Ya = 2, Yb = 1, Yc = 4, Yd = 3.
-->

List all tuples:

```logtalk
forall(brick_stack::tuple(Tuple), (write(Tuple), nl)).
```

<!--
[c,d]
[a,b]
[d,a]

true.
-->

Clean up instances, tuples and monitors:

```logtalk
brick_stack::remove_all_tuples.
```

<!--
true.
-->

Unset `stack_monitor` as an event monitor:

```logtalk
after_event_registry::del_monitors(_, _, _, stack_monitor).
```

<!--
true.
-->

Delete all bricks:

```logtalk
brick::delete_all.
```

<!--
true.
-->
