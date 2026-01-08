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

# complements (allow)

This folder contains an example that show how to use both static and dynamic
categories to explicitly complement an existing object compiled with the
`complements` flag set to `allow`. With this setting, a category can both
redefine and add new functionality to the complemented object.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the non-patched `employee` object:

```logtalk
logtalk_load(complements_allow(employee)).
```

Ask the `employee` object its name:

```logtalk
employee::name(Name).
```

<!--
Name = john.
-->

Ask the `employee` object its salary:

```logtalk
employee::salary(Salary).
```

<!--
Salary = 23500.
-->

Check the protocol of the `employee` object:

```logtalk
findall(Predicate, employee::current_predicate(Predicate), Predicates).
```

<!--
Predicates = [age/1, name/1, salary/1].
-->

Now load the `add_on` complementing category:

```logtalk
logtalk_load(complements_allow(add_on)).
```

Load the `dynamic.lgt` source file that creates a dynamic complementing
category, also patching the `employee` object:

```logtalk
logtalk_load(complements_allow(dynamic)).
```

Find categories that complement objects:

```logtalk
%%table
complements_object(Category, Object).
```

<!--
Category = dynamic_patch, Object = employee ;
Category = add_on, Object = employee ;
false.
-->

Use the event handler defined in the `add_on` category for the `employee` object:

```logtalk
employee::name(Name).
```

<!--
Received message name(_16) from user
Name = john.
-->

Check the consequences of the runtime patch of the `salary/1` predicate:

```logtalk
employee::salary(Salary).
```

<!--
Received message salary(_G192) from user
Salary = 42000.
-->

Check the new protocol of the `employee` object:

```logtalk
employee::predicates(Predicates).
```

<!--
Received message predicates(_G180) from user

Predicates = [after/3, age/1, before/3, income/1, name/1, predicates/1, salary/1].
-->

List the properties of the `predicates/1` predicate:

```logtalk
%%table
employee::predicate_property(predicates(_), Property).
```

<!--
Property = logtalk ;
Property = scope(public) ;
Property = (public) ;
Property = static ;
Property = declared_in(add_on) ;
Property = declared_in(add_on, 29) ;
Property = defined_in(add_on) ;
Property = defined_in(add_on, 31) ;
Property = number_of_clauses(1).
-->

List the properties of the `income/1` predicate:

```logtalk
%%table
employee::predicate_property(income(_), Property).
```

<!--
Property = logtalk ;
Property = scope(public) ;
Property = (public) ;
Property = static ;
Property = declared_in(employee) ;
Property = alias_of(salary(_G3724)) ;
Property = defined_in(dynamic_patch) ;
Property = number_of_clauses(0).
-->

Later, the boss finds out about the employee hacked salary:

```logtalk
abolish_category(dynamic_patch).
```

<!--
true.
-->

The employee salary is thus back to its original value:

```logtalk
employee::salary(Salary).
```

<!--
Received message salary(_G192) from user
Salary = 23500.
-->
