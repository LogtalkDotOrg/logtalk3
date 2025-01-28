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

# predicate_lookups

This example illustrates the predicate declaration and predicate definition
lookup algorithms used when sending a message to an object. For full details,
see the Handbook section on inheritance.

The lookup algorithms differ for instances and for prototypes and also depend
if the lookup is for a predicate declaration or for a predicate definition.
Sending a message to an object, requires two predicate lookups (performed at
compile-time, when possible):

- Lookup the predicate declaration to check that the predicate in within
the scope of the *sender*. In the most common cases, this means that the
predicate is declared public.

- Assuming that the predicate exists and is within scope, lookup the
predicate definition to answer the message. If none found, the message
simply fails as per the Closed World Assumption (CWA).

See the comments in the `prototypes.lgt` and `classes.lgt` source files
for further details.

Start by loading the example:

```logtalk
logtalk_load(predicate_lookups(loader)).
```

When sending a message to a prototype, the lookup for the predicate
declaration starts at the prototype itself; therefore, the following
message is valid:

```logtalk
bike::frame(Material).
```

<!--
Material = aluminum.
-->

When a prototype doesn't contain a declaration for the predicate in
the message, the lookup continues in the prototype parent(s):

```logtalk
bike::where(Where).
```

<!--
Where = land.
-->

When sending a message to a prototype, the lookup for the predicate
definition starts at the prototype itself:

```logtalk
mountain_bike::frame(Material).
```

<!--
Material = carbon.
-->

When a prototype doesn't contain a definition for the predicate in
the message, the lookup continues in the prototype parent(s):

```logtalk
mountain_bike::crewed.
```

We can use the built-in reflection predicates to query about predicate
declarations and definitions; for example:

```logtalk
%%table
mountain_bike::current_predicate(Predicate).
```

<!--
Predicate = crewed/0 ;
Predicate = frame/1 ;
Predicate = where/1 ;
false.
-->

```logtalk
%%table
mountain_bike::predicate_property(frame(_), Property).
```

<!--
Property = logtalk ;
Property = scope(public) ;
Property =  (public) ;
Property = static ;
Property = declared_in(bike) ;
Property = declared_in(bike, 37) ;
Property = defined_in(mountain_bike) ;
Property = defined_in(mountain_bike, 49) ;
Property = redefined_from(bike) ;
Property = redefined_from(bike, 39) ;
Property = number_of_clauses(1) ;
Property = number_of_rules(0)
true.
-->

When sending a message to an instance, the lookup for the predicate
declaration starts at the instance class; therefore, the following
message is valid:

```logtalk
paraglider::structure(Structure).
```

<!--
Structure = soft.
-->

Note that the lookup for the predicate definition starts in the
instance itself, resulting in the `structure(soft)` answer; 

When the instance class doesn't contain a declaration for the predicate
in the message, the lookup continues in the class superclasses; therefore,
the following message is also valid:

```logtalk
sailplane::purpose(Purpose).
```

<!--
Purpose = fun.
-->

When the instance doesn't contain a definition for the predicate in the
message, the lookup continues in the class and, if not found there, in
the class superclasses:

```logtalk
sailplane::structure(Structure).
```

<!--
Structure = rigid.
-->

We can use the built-in reflection predicates to query about predicate
declarations and definitions; for example:

```logtalk
%%table
sailplane::current_predicate(Predicate).
```

<!--
Predicate = purpose/1 ? ;
Predicate = structure/1 ? ;
false.
-->

```logtalk
%%table
sailplane::predicate_property(purpose(_), Property).
```

<!--
Property = logtalk ? ;
Property = scope(public) ? ;
Property = public ? ;
Property = static ? ;
Property = declared_in(artificial) ? ;
Property = declared_in(artificial,40) ? ;
Property = defined_in(sailplane) ? ;
Property = defined_in(sailplane,74) ? ;
Property = redefined_from(aircraft) ? ;
Property = redefined_from(aircraft,53) ? ;
Property = number_of_clauses(1) ? ;
Property = number_of_rules(0)
true.
-->
