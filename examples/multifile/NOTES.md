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

# multifile

This example illustrates how to use multifile predicates/non-terminals within
Logtalk objects and categories.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(multifile(loader)).
```

Call the `a/1` multifile predicate:

```logtalk
%%table
main::a(X).
```

<!--
X = 1 ;
X = 2 ;
X = 3 ;
X = 4 ;
X = 5.
-->

Call the `b/1` multifile predicate:

```logtalk
%%table
main::b(X).
```

<!--
X = one ;
X = two ;
X = three.
-->

Call the `nt//1` multifile non-terminal:

```logtalk
%%table
logtalk << phrase(main::nt(X), [1,2,3], Rest).
```

<!--
X = 1, Rest = [2, 3] ;
X = end, Rest = [1, 2, 3].
-->
