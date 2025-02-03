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

# aliases

For a description of this example, please see the comments in the
`aliases.lgt` source file.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(aliases(loader)).
```

Check the object `square(_)` public protocol:

```logtalk
%%table
square(_)::current_predicate(Predicate).
```

<!--
Predicate = side/1 ;
Predicate = width/1 ;
Predicate = height/1 ;
Predicate = area/1 ;
false.
-->

Test the `side/1` alias:

```logtalk
square(2)::side(Side).
```

<!--
Side = 2.
-->

Enumerate the properties of the `side/1` predicate alias:

```logtalk
%%table
square(2)::predicate_property(side(_), Property).
```

<!--
Property = alias_of(width(_56130)) ;
Property = alias_declared_in(square(2)) ;
Property = alias_declared_in(square(2), 52) ;
Property = logtalk ;
Property = scope(public) ;
Property =  (public) ;
Property = static ;
Property = declared_in(rectangle(_56350, _56352)) ;
Property = declared_in(rectangle(_56350, _56352), 32) ;
Property = defined_in(rectangle(_63112, _63112)) ;
Property = defined_in(rectangle(_63942, _63942), 36) ;
Property = number_of_clauses(1) ;
Property = number_of_rules(0).
-->

Enumerate the properties of the `width/1` predicate:

```logtalk
%%table
square(2)::predicate_property(width(_), Property).
```

<!--
Property = logtalk ;
Property = scope(public) ;
Property =  (public) ;
Property = static ;
Property = declared_in(rectangle(_3434, _3436)) ;
Property = declared_in(rectangle(_3434, _3436), 32) ;
Property = defined_in(rectangle(_7916, _7916)) ;
Property = defined_in(rectangle(_8742, _8742), 36) ;
Property = number_of_clauses(1) ;
Property = number_of_rules(0).
-->

Check the object `circle(_)` public protocol:

```logtalk
%%table
circle(_)::current_predicate(Predicate).
```

<!--
Predicate = r/1 ;
Predicate = rx/1 ;
Predicate = ry/1 ;
Predicate = area/1 ;
false.
-->

Test the `r/1` alias:

```logtalk
circle(3)::r(Radius).
```

<!--
Radius = 3.
-->

Enumerate the properties of the `r/1` predicate alias:

```logtalk
%%table
circle(3)::predicate_property(r(_), Property).
```

<!--
Property = alias_of(rx(_33998)) ;
Property = alias_declared_in(circle(3)) ;
Property = alias_declared_in(circle(3), 89) ;
Property = logtalk ;
Property = scope(public) ;
Property =  (public) ;
Property = static ;
Property = declared_in(ellipse(_34218, _34220)) ;
Property = declared_in(ellipse(_34218, _34220), 69) ;
Property = defined_in(ellipse(_40980, _40980)) ;
Property = defined_in(ellipse(_41810, _41810), 73) ;
Property = number_of_clauses(1) ;
Property = number_of_rules(0).
-->

Enumerate the properties of the `rx/1` predicate:

```logtalk
%%table
circle(3)::predicate_property(rx(_), Property).
```

<!--
Property = logtalk ;
Property = scope(public) ;
Property =  (public) ;
Property = static ;
Property = declared_in(ellipse(_46416, _46418)) ;
Property = declared_in(ellipse(_46416, _46418), 69) ;
Property = defined_in(ellipse(_50898, _50898)) ;
Property = defined_in(ellipse(_51724, _51724), 73) ;
Property = number_of_clauses(1) ;
Property = number_of_rules(0).
-->
