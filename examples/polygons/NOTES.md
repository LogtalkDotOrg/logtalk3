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

# polygons

In this example we have several types of polygons that can be concentric.
This is represented by a concentric binary relation ensuring that whenever 
a polygon is moved towards a new position, all polygons likewise concentric 
are moved along with them.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(polygons(loader)).
```

First create four polygons and move each one to a different position:

```logtalk
triangle::new(t, [position-(4, 5)]).
```

<!--
true.
-->

```logtalk
square::new(s, [position-(3, 2)]).
```

<!--
true.
-->

```logtalk
pentagon::new(p, [position-(7, 1)]).
```

<!--
true.
-->

```logtalk
hexagon::new(h, [position-(2, 4)]).
```

<!--
true.
-->

Create two tuples of relation concentric:

```logtalk
concentric::add_tuple([t, s]).
```

<!--
true.
-->

```logtalk
concentric::add_tuple([p, h]).
```

<!--
true.
-->

Check results:

```logtalk
concentric::tuple(Tuple), write(Tuple), nl, fail; true.
```

<!--
[t,s]
[p,h]

true.
-->

```logtalk
t::position(Xt, Yt), s::position(Xs, Ys), p::position(Xp, Yp), h::position(Xh, Yh).
```

<!--
Xh = 7, Yh = 1, Xp = 7, Xs = 4, Xt = 4, Yp = 1, Ys = 5, Yt = 5.
-->

```logtalk
after_event_registry::monitors(Ma).
```

<!--
Ma = [concentric].
-->

Move the triangle and the hexagon to new positions:

```logtalk
t::move(3, 3), h::move(8, 4).
```

<!--
true.
-->

Check results:

```logtalk
concentric::tuple(Tuple), write(Tuple), nl, fail; true.
```

<!--
[t,s]
[p,h]

true.
-->

```logtalk
t::position(Xt, Yt), s::position(Xs, Ys), p::position(Xp, Yp), h::position(Xh, Yh).
```

<!--
Xh = 8, Yh = 4, Xp = 8, Xs = 3, Xt = 3, Yp = 4, Ys = 3, Yt = 3.
-->

```logtalk
after_event_registry::monitors(Ma).
```

<!--
Ma = [concentric].
-->

Create another tuple of relation concentric:

```logtalk
concentric::add_tuple([t, p]).
```

<!--
true.
-->

Move the pentagon to a new position:

```logtalk
p::move(2, 7).
```

<!--
true.
-->

Check results:

```logtalk
concentric::tuple(Tuple), write(Tuple), nl, fail; true.
```

<!--
[t,s]
[p,h]
[t,p]

true.
-->

```logtalk
t::position(Xt, Yt), s::position(Xs, Ys), p::position(Xp, Yp), h::position(Xh, Yh).
```

<!--
Xh = 2, Yh = 7, Xp = 2, Xs = 2, Xt = 2, Yp = 7, Ys = 7, Yt = 7.
-->

List _after_ event monitors:

```logtalk
after_event_registry::monitors(Monitors).
```

<!--
Monitors = [concentric].
-->

Clean up instances, tuples and monitors:

```logtalk
concentric::remove_all_tuples.
```

<!--
true.
-->

```logtalk
triangle::delete(t).
```

<!--
true.
-->

```logtalk
square::delete(s).
```

<!--
true.
-->

```logtalk
pentagon::delete(p).
```

<!--
true.
-->

```logtalk
hexagon::delete(h).
```

<!--
true.
-->
