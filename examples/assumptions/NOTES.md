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

# assumptions

This example provides a simple, limited, implementation of ground linear
and intuitionistic assumptions as discussed in the following paper:

```text
@inproceedings{DBLP:conf/slp/TarauDF95,
	author    = {Paul Tarau and Ver{\'o}nica Dahl and Andrew Fall},
	title     = {Backtrackable State with Linear Assumptions, Continuations and Hidden Accumulator Grammars},
	booktitle = {ILPS},
	year      = {1995},
	pages     = {642},
	crossref  = {DBLP:conf/slp/1995},
	bibsource = {DBLP, http://dblp.uni-trier.de}
}

@proceedings{DBLP:conf/slp/1995,
	editor    = {John W. Lloyd},
	title     = {Logic Programming, Proceedings of the 1995 International Symposium, Portland, Oregon, USA, December 4-7, 1995},
	booktitle = {ILPS},
	publisher = {MIT Press},
	year      = {1995},
	isbn      = {0-262-62099-5},
	bibsource = {DBLP, http://dblp.uni-trier.de}
}
```

A web version of this paper is also available at:

http://www.cse.unt.edu/~tarau/research/PapersHTML/state.html

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(assumptions(loader)).
```

Paths example:

```logtalk
%%table
paths::init, paths::path(1, 5, Path).
```

<!--
Path = [1, 2, 4, 5] ;
Path = [1, 3, 5] ;
false.
-->

Switch example

```logtalk
switch::test(2).
```

<!--
two
true.
-->

```logtalk
switch::test(1).
```

<!--
one
true.
-->

```logtalk
switch::test(4).
```

<!--
unexpected(4)
true.
-->

Grades example:

```logtalk
grades::(assumel(take(hans, german)), grade(hans)).
```

<!--
true.
-->

```logtalk
grades::(assumel(take(hans, italian)), grade(hans)).
```

<!--
false.
-->
