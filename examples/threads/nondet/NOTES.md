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

# threads - nondet

This folder illustrates non-deterministic multi-threading calls and the
use of tags to distinguish between multi-threading calls.

Start by loading the example:

```logtalk
logtalk_load(nondet(loader)).
```

Mke a threaded call with a non-deterministic goal:

```logtalk
threaded_call(lists::member(X, [1,2,3])).
```

<!--
X = _G189. 
-->

Retrieve through backtracking all solutions for the non-deterministic goal:

```logtalk
%%table
threaded_exit(lists::member(X, [1,2,3])).
```

<!--
X = 1 ;
X = 2 ;
X = 3 ;
false.
-->

Make a threaded call by committing to the first solution found:

```logtalk
threaded_once(lists::member(X, [1,2,3])).
```

<!--
X = _G189. 
-->

Retrieve through backtracking the goal solution:

```logtalk
threaded_exit(lists::member(X, [1,2,3])).
```

<!--
X = 1 ;
false.
-->

When two or more variant calls are made...

```logtalk
threaded_call(lists::member(X, [1,2,3])), threaded_call(lists::member(Y, [1,2,3])).
```

<!--
X = _G189 Y =_G190. 
-->

...the first `threaded_exit/1` call will pick one of them:

```logtalk
%%table
threaded_exit(lists::member(X, [1,2,3])).
```

<!--
X = 1 ;
X = 2 ;
X = 3 ;
false.
-->

...and a second threaded_exit/1 call will pick the remaining one:

```logtalk
%%table
threaded_exit(lists::member(X, [1,2,3])).
```

<!--
X = 1 ;
X = 2 ;
X = 3 ;
false.
-->

Tags may be used to distinguish between threaded calls if needed:

```logtalk
threaded_call(lists::member(X, [1,2,3]), Tag).
```

<!--
Tag = 1. 
-->

```logtalk
threaded_call(lists::member(X, [1,2,3]), Tag).
```

<!--
Tag = 2. 
-->

```logtalk
%%table
threaded_exit(lists::member(X, [1,2,3]), 2).
```

<!--
X = 1 ;
X = 2 ;
X = 3 ;
false.
-->

Use a subsumed goal instead of a variant of the original goal:

```logtalk
threaded_call(lists::member(X, [1,2,3,2])).
```

<!--
X = _G189. 
-->

```logtalk
threaded_exit(lists::member(2, [1,2,3,2])).
```

<!--
More ;
More ;
false.
-->
