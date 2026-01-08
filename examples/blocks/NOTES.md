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
-->

# blocks

This folder contains an example of representation and handling of relations
using events. We have instances of class `block` and a binary `block_stack`
relation between the blocks. Every time we move a block, we want the blocks
on top of it to move along. If we break the stack by moving a middle block,
we want to automatically destroy the corresponding relation tuple.

It's instructive to use the debugger to better understand this example.
Set spy points in all block instances and then activate the debugger.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(blocks(loader)).
```

Create four blocks, all standing on the "ground" (use your imagination... ;-)
(don't use message broadcasting syntax in order to workaround a XSB parser bug):

```logtalk
block::new(a, [position-(8, 1)]), block::new(b, [position-(6, 1)]), block::new(c, [position-(4, 1)]), block::new(d, [position-(2, 1)]).
```

<!--
true.
-->

Set up an ASCII stack monitor so we can watch the blocks moving:

```logtalk
define_events(after, _, move(_,_), _, stack_monitor).
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
block_stack::add_tuple(c-d), block_stack::add_tuple(b-c), block_stack::add_tuple(a-b).
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
block_stack::tuples(Tuples).
```

<!--
Tuples = [c-d, b-c, a-b].
-->

List all _after_ events and monitors:

```logtalk
%%table
current_event(after, Object, Message, Sender, Monitor).
```

<!--
Message = move(_, _), Monitor = stack_monitor ;
Object = d, Message = move(_, _), Monitor = block_stack ;
Object = c, Message = move(_, _), Monitor = block_stack ;
Object = a, Message = move(_, _), Monitor = block_stack ;
Object = b, Message = move(_, _), Monitor = block_stack ;
false.
-->

Move all stack to new position by moving bottom block:

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
block_stack::tuples(Tuples).
```

<!--
Tuples = [c-d, b-c, a-b].
-->

Break stack in half by moving b to the "ground":

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
block_stack::tuples(Tuples).
```

<!--
Tuples = [c-d, a-b].
-->

Create a new `block_stack` tuple:

```logtalk
block_stack::add_tuple(d-a).
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
block_stack::tuples(Tuples).
```

<!--
Tuples = [c-d, a-b, d-a].
-->

Move all stack to new position by moving bottom block:

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
block_stack::tuples(Tuples).
```

<!--
Tuples = [c-d, a-b, d-a].
-->

Clean up instances, tuples and monitors:

```logtalk
block_stack::remove_all_tuples.
```

<!--
true.
-->

Delete all blocks:

```logtalk
block::delete_all.
```

<!--
true.
-->
