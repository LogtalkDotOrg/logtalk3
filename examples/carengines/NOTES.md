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

# carengines

For a description of this example, please see the comments in the
`carengines.lgt` source file.

Start by loading the example:

```logtalk
logtalk_load(carengines(loader)).
```

Both cars provide the same interface, declared in the protocol
that is implemented by the categories imported by each object.

Public predicates of the `sedan` object:

```logtalk
%%table
sedan::current_predicate(Predicate).
```

<!--
P = reference/1 ;
P = capacity/1 ;
P = cylinders/1 ;
P = horsepower_rpm/2 ;
P = bore_stroke/2 ;
P = fuel/1 ;
false.
-->

Public predicates of the `coupe` object:

```logtalk
%%table
coupe::current_predicate(Predicate).
```

<!--
P = reference/1 ;
P = capacity/1 ;
P = cylinders/1 ;
P = horsepower_rpm/2 ;
P = bore_stroke/2 ;
P = fuel/1 ;
false.
-->

The `sedan` engine properties are the ones defined in the corresponding 
imported category (`classic`):

```logtalk
sedan::(reference(Name), cylinders(Cylinders), horsepower_rpm(HP, RPM)).
```

<!--
Name = 'M180.940', Cylinders = 6, HP = 94, RPM = 4800.
-->

The `coupe` engine properties are the ones defined in the corresponding 
imported category (`sport`) plus the ones inherited from the top category 
(`classic`) which are not overridden:

```logtalk
coupe::(reference(Name), cylinders(Cylinders), horsepower_rpm(HP, RPM)).
```

<!--
Name = 'M180.941', Cylinders = 6, HP = 115, RPM = 3657.
-->
