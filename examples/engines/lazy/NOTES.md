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

# engines - lazy

This example requires support for both threads and coroutining. Currently it
runs on ECLiPSe and SWI-Prolog. It should run also on XSB and YAP if and when
these systems bugs with coroutining and/or threads get fixed.

This folder contains an example of using threaded engines and coroutining to
implement a lazy alternative to the standard `findall/3` meta-predicate. This
lazy version is described in the following paper:

```bibtex
@inbook{Tarau2000,
	author="Tarau, Paul",
	editor="Lloyd, John and Dahl, Veronica and Furbach, Ulrich and Kerber, Manfred and Lau, Kung-Kiu and Palamidessi, Catuscia and Pereira, Lu{\'i}s Moniz and Sagiv, Yehoshua and Stuckey, Peter J.",
	chapter="Fluents: A Refactoring of Prolog for Uniform Reflection and Interoperation with External Objects",
	title="Computational Logic --- CL 2000: First International Conference London, UK, July 24--28, 2000 Proceedings",
	year="2000",
	publisher="Springer Berlin Heidelberg",
	address="Berlin, Heidelberg",
	pages="1225--1239",
	isbn="978-3-540-44957-7",
	doi="10.1007/3-540-44957-4_82",
	url="http://dx.doi.org/10.1007/3-540-44957-4_82"
}
```

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(lazy(loader)).
```

Return a lazy list and access its elements:

```logtalk
lazy::find_all(X, (repeat,random::random(X)), List), list::member(E, List).
```

<!--
List = [0.915656206971831|_G118],
E = 0.915656206971831,
freeze(_G118, '$lazy#0.source_lazy_list#2'(4, _G118, <lazy,user,lazy,lazy,c(user,user,r(user,lazy,[],[]))-[(repeat,random::random(X))],[],>)) ;

List = [0.915656206971831, 0.6669572934854013|_G155],
E = 0.6669572934854013,
freeze(_G155, '$lazy#0.source_lazy_list#2'(4, _G155, <lazy,user,lazy,lazy,c(user,user,r(user,lazy,[],[]))-[(repeat,random::random(X))],[],>)) ;

List = [0.915656206971831, 0.47712105608919275, 0.5965100813402789|_G194],
E = 0.5965100813402789,
freeze(_G194, '$lazy#0.source_lazy_list#2'(4, _G194, <lazy,user,lazy,lazy,c(user,user,r(user,lazy,[],[]))-[(repeat,random::random(X))],[],>)) ;
List = [0.915656206971831, 0.47712105608919275, 0.14210821770124227, 0.20944855618709624|_G395],

...
-->

```logtalk
lazy::find_all(X, (repeat,random::random(X)), List), list::nth1(N, List, E).
```

<!--
List = [0.09230089279334841|_G3527],
N = 1,
E = 0.09230089279334841,
freeze(_G3527, '$lazy#0.source_lazy_list#2'(1, _G3527, <lazy,user,lazy,lazy,c(user,user,r(user,lazy,[],[]))-[(repeat,random::random(X))],[],>)) ;

List = [0.09230089279334841, 0.4435846174457203|_G3589],
N = 2,
E = 0.4435846174457203,
freeze(_G3589, '$lazy#0.source_lazy_list#2'(1, _G3589, <lazy,user,lazy,lazy,c(user,user,r(user,lazy,[],[]))-[(repeat,random::random(X))],[],>)) ;

List = [0.09230089279334841, 0.7230402056221108, 0.94581636451987|_G3651],
N = 3,
E = 0.94581636451987,
freeze(_G3651, '$lazy#0.source_lazy_list#2'(1, _G3651, <lazy,user,lazy,lazy,c(user,user,r(user,lazy,[],[]))-[(repeat,random::random(X))],[],>)) ;

...
-->
