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

# prototypes

This is a simple example illustrating the concept of prototypes, which provide
an alternative to classes for object-oriented programming, using characters
from the Alf television sitcom. Logtalk supports both classes and prototypes.
It can be used as either a prototype-based language or a class-based language.
Moreover, prototypes and classes can be freely used in the same application.

Class-based OOP languages are far more popular and, unfortunately, many books
and teaching materials equate OOP languages with class-based OOP languages.
Some examples of other prototype-based OOP languages include Self, JavaScript,
Io, NewtonScript, Lua, and Slate.

Start by loading the example:

```logtalk
logtalk_load(prototypes(loader)).
```

Ask Alf about its attributes:

```logtalk
forall(alf::current_predicate(F/A), (functor(P,F,A), alf::P, writeq(P), nl)).
```

<!--
chases('Lucky')
favorite_food(cats)
motto('Are you going to finish that sandwich?')
name('Gordon Shumway')
planet('Melmac')
stomachs(8)

true.
-->

Ask Rhonda about its love affairs:

```logtalk
rhonda::boyfriend(Melmacian).
```

<!--
Melmacian = alf.
-->
