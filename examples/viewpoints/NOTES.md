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

# viewpoints

Example adapted from the chapter "Classifying Prototype-Based Programming
Languages" by Christophe Dony, Jacques Malenfant, and Daniel Bardou, found 
on the book "Prototype-Based Programming - Concepts, Languages, and 
Applications" published by Springer.

This prototype programming example illustrates how we can do both property 
sharing and value sharing in Logtalk by calling the built-in predicate 
modification methods `asserta/1`, `assertz/1`, and `retract/1` either in
the context of _this_ or in the context of _self_.

In this example we have a prototype, `joe_person`, containing general data
on Joe such as its age, name, or address, and four descendant prototypes
or viewpoints, `joe_sportsman`, `joe_employee`, `joe_chess_player`, and
`joe_film_enthusiast`. Each descendant contains data related to a particular
viewpoint about Joe.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(viewpoints(loader)).
```

<!--
true.
-->

We can start by asking joe its age:

```logtalk
joe_person::age(Age).
```

<!--
Age = 30.
-->

The same question could be made via any of its viewpoints:

```logtalk
joe_sportsman::age(Age).
```

<!--
Age = 30
-->

Now let's tell joe to get older:

```logtalk
joe_person::grow_older.
```

<!--
true.
-->

We can verify the effect of the above message from any of the viewpoints:

```logtalk
joe_chess_player::age(Age).
```

<!--
Age = 31
-->

Because the `growOld/0` and the `age/1` predicates are implemented using 
property sharing, we can send the `grow_older/0` message to any viewpoint:

```logtalk
joe_employee::grow_older.
```

<!--
true.
-->

We can check this by asking joe its age:

```logtalk
joe_person::age(Age).
```

<!--
Age = 32
-->

As you can see, although the modification message have been sent to a 
descendant, its the predicate `age/1` in the parent that got updated.

To illustrate value sharing we use a couple of predicates, `score/1`
and `set_score/0`, defined in `joe_person`:

```logtalk
joe_person::score(Score).
```

<!--
Score = 0.
-->

Initially, `score/1` is only defined for `joe_person`, so every descendant 
or viewpoint will share its value/definition:

```logtalk
joe_employee::score(Score).
```

<!--
Score = 0.
-->

But if we decide to increment the counter by sending the `set_score/0` message
to a descendant (don't use message broadcasting syntax in order to workaround
a XSB parser bug):

```logtalk
joe_chess_player::set_score(2200), joe_chess_player::score(Score).
```

<!--
Score = 2200.
-->

Then the descendant will now have a local definition for `counter/1`,
independent of the definition in its parent, `joe_person`:

```logtalk
joe_person::score(Score).
```

<!--
Score = 0.
-->

The other descendants/viewpoints will continue to share the definition 
in `joe_person`:

```logtalk
joe_sportsman::score(Score).
```

<!--
Score = 0.
-->
