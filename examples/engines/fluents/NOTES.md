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

# engines - fluents

This example requires support for both threads and coroutining. Currently it
runs on ECLiPSe and SWI-Prolog. It should run also on XSB and YAP if and when
these systems bugs with coroutining and/or threads get fixed.

This folder contains examples of fluents implemented using threaded engines.
Fluents are described in the paper:

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

Load the example:

```logtalk
logtalk_load(fluents(loader)).
```

<!--
true.
-->

Get answers from the fluent:

```logtalk
fluents::next(N1).
```

<!--
N1 = 1.
-->

```logtalk
fluents::next(N2).
```

<!--
N2 = 2.
-->

```logtalk
fluents::next(N3).
```

<!--
N3 = 3.
-->

After exhausting the fluent answers, subsequent queries fail
until the threaded engine implementing the fluent is destroyed:

```logtalk
fluents::next(_).
```

<!--
false.
-->

```logtalk
fluents::next(_).
```

<!--
false.
-->
