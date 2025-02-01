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

# family_alt

This folder contains an alternative implementation of the `family` example,
which in turn is a version of the classical family tree Prolog example.

This alternative solution defines a `family/1` parametric object, providing
predicates for common family relations like `sister/2` and `father/2`. This
parametric object allows concrete families to be plugged-in by defining the 
basic female, male, and parent relations as multifile predicates parameterized
by the family database object. Several family objects can be loaded at the
same time. Using a parametric object to query a family extended relations is
convenient as the object parameter allows us to make the name of the concrete
family easily available to any predicate.

The main advantage of this solution is that it avoids dynamic binding as
found in the `family` example. Moreover, when this version is compiled in
optimized mode, the multifile predicate clauses, which act as liking clauses,
are inlined. The main downside of this solution is the boilerplate code that
must be written (i.e., the multifile predicate definitions).

Load the example:

```logtalk
logtalk_load(family_alt(loader)).
```

Some example queries using the Addams family relations:

```logtalk
%%table
family(addams)::sister(Sister, Sibling).
```

<!--
Sister = wednesday, Sibling = pubert ;
Sister = wednesday, Sibling = pugsley ;
Sister = wednesday, Sibling = pubert ;
Sister = wednesday, Sibling = pugsley ;
false.
-->

Some example queries using the Simpsons family relations:

```logtalk
%%table
family(simpsons)::mother(Mother, Child).
```

<!--
Mother = marge, Child = bart ;
Mother = marge, Child = lisa ;
Mother = marge, Child = maggie ;
false.
-->

Some example queries using the extended Simpsons family relations:

```logtalk
%%table
family(simpsons_extended)::parent(Parent, Child).
```

<!--
Parent = homer, Child = bart ;
Parent = homer, Child = lisa ;
Parent = homer, Child = maggie ;
Parent = marge, Child = bart ;
Parent = marge, Child = lisa ;
Parent = marge, Child = maggie ;
Parent = abe, Child = homer ;
Parent = abe, Child = herb ;
Parent = gaby, Child = herb ;
Parent = mona, Child = homer ;
false.
-->
