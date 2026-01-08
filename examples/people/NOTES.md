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

# people

This example illustrates how to define object constructors for a simple
hierarchy of objects representing persons, students, and teachers. For
simplicity, prototypes are used instead of classes. Logtalk provides a
low-level, built-in predicate, `create_object/4`, for dynamically creating
new objects. This predicate can be used to define object constructors,
similar to those used in other OOP languages.

This example also illustrates how to efficiently represent objects with
immutable state using parametric objects and object proxies (Prolog facts;
see also the `proxies` example). This alternative is often a good choice
when dealing with a very large number of objects due to its compact
representation and/or connecting Prolog data tables with a Logtalk object
hierarchies.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(people(loader)).
```

Create two new persons:

```logtalk
person::new(Id1, 'Oscar the Grouch', '1969/11/10'), person::new(Id2, 'Cookie Monster', '1969/12/02').
```

<!--
Id1 = o1, Id2 = o2.
-->

Print a description of a person:

```logtalk
o2::print.
```

<!--
Name:      Oscar the Grouch
Birth:     1969/11/10
true.
-->

Create a new teacher and a new student:

```logtalk
teacher::new(Id3, 'Gordon Robinson', '1969/11/10', '3.2'), student::new(Id4, 'Roosevelt Franklin', '1969/11/10', 'Blue').
```

<!--
Id3 = o3, Id4 = o4.

Print a description of a student:

```logtalk
o4::print.
```

<!--
Name:      Roosevelt Franklin
Birth:     1969/11/10
Dormitory: Blue
true.
-->

Try the alternative object representations using the object proxies:

```logtalk
{student('Roosevelt Franklin', _, _)}::print.
```

<!--
Name:      Roosevelt Franklin
Birth:     1969/11/10
Dormitory: Blue
true.
-->
