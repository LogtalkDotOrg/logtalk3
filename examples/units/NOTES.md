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

# units

This folder contains a Logtalk version of a GNU Prolog/CX unit example, a
simple dictionary implementation using a list of key-value pairs:

```logtalk
%%highlight
:- unit(dict(ST)).

dict(ST).

lookup(KEY, VALUE) :- ST=[KEY=VALUE|_].
lookup(KEY, VALUE) :- ST=[_|STx], dict(STx) :> lookup(KEY, VALUE).
```

For a description of GNU Prolog/CX, see:

```text
@inproceedings{inproceedings,
	author = {Abreu, Salvador and Diaz, Daniel},
	year = {2003},
	month = {12},
	pages = {128-147},
	title = {Objective: In Minimum Context},
	isbn = {978-3-540-20642-2},
	doi = {10.1007/978-3-540-24599-5_10}
}
```

The code example is also mentioned in the following paper:

```text
@incollection{pmoura11a,
	author = {Paulo Moura},
	booktitle = {Applications of Declarative Programming and Knowledge Management},
	title = {Programming Patterns for Logtalk Parametric Objects},
	editor = "Salvador Abreu and Dietmar Seipel",
	series = "Lecture Notes in Artificial Intelligence",
	volume = "6547",
	publisher = "Springer-Verlag",
	address = "Berlin Heidelberg",
	pages = "52--69",
	month = apr,
	year = 2011
}
```

The Logtalk compiler optimizes message-sending calls sent from an object
to itself (as found in the definition of the `lookup/2` predicate in this
example), removing the message-sending overhead, and thus providing the
same performance as a local predicate call.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(units(loader)).
```

Get the dict:

```logtalk
dict([a=1,b=2,c=3,d=4,e=5])::dict(Dict).
```

<!--
Dict = [a=1,b=2,c=3,d=4,e=5].
-->

Perform a dict lookup:

```logtalk
dict([a=1,b=2,c=3,d=4,e=5])::lookup(c, C).
```

<!--
C = 3.
-->
