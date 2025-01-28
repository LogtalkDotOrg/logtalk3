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

# sicstus

The examples in this folder are adapted from the SICStus Prolog manual.

Start by loading the example and necessary library supporting files:

```logtalk
logtalk_load(sicstus(loader)).
```

Try some simple queries:

```logtalk
sort(rational)::sort([1/8, 2/7, 6/5, 2/9, 1/3], Sorted).
```

<!--
Sorted = [1/8, 2/9, 2/7, 1/3, 6/5].
-->

```logtalk
sort(colours)::sort([orange, indigo, red, yellow, violet, blue, green], Sorted).
```

<!--
Sorted = [red, orange, yellow, green, blue, indigo, violet].
-->

Using the pseudo-object user implies using the Prolog built-in standard 
order operators:

```logtalk
sort(user)::sort([3, 1, 4, 2, 9], Sorted).
```

<!--
Sorted = [1, 2, 3, 4, 9].
-->

Some messages testing object parameter passing and using:

```logtalk
red_circle(3)::color(Color).
```

<!--
Color = red.
-->

```logtalk
red_circle(3)::area(Area).
```

<!--
Area = 28.274334.
-->

```logtalk
red_circle(3)::ancestors(As).
```

<!--
As = [circle(3, red), ellipse(3, 3, red)].
-->

Show the execution context using a method (context/3) that is defined in the
hierarchy root and specialized (using super calls) in each descendant:

```logtalk
red_circle(3)::context.
```

<!--
red_circle1
self: red_circle(3)
this: red_circle(3)
sender: user

circle2
self: red_circle(3)
this: circle(3,red)
sender: user

ellipse3
self: red_circle(3)
this: ellipse(3,3,red)
sender: user

true.
-->

Send a conjunction of messages/goals to an object; just some useful syntax sugar
(don't use message broadcasting syntax in order to workaround a XSB parser bug):

```logtalk
square(2)::side(Side), square(2)::width(Width), square(2)::height(Height), square(2)::area(Area).
```

<!--
Side = 2, Width = 2, Height = 2, Area = 4.
-->

Find all messages accepted by an object:

```logtalk
%%table
square(2)::current_predicate(Pred).
```

<!--
Pred = side/1 ; 
Pred = width/1 ;
Pred = height/1 ;
Pred = area/1 ;
false.

Find all data on a specific message accepted by an object:

```logtalk
%%table
square(_)::predicate_property(side(_), Prop).
```

<!--
Prop = public ;
Prop = static ;
Prop = declared_in(square(_133)) ;
Prop = defined_in(square(_164)).
-->
