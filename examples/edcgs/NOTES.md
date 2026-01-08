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

# edcgs

For a description of this example, please see the comments in the example
source files and the original individual examples by Peter Van Roy at:

https://www2.eecs.berkeley.edu/Pubs/TechRpts/1990/CSD-90-583.pdf

Some examples and tests written by Michael Hendricks for his `edcg`
SWI-Prolog pack repo at:

https://github.com/mndrix/edcg

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(edcgs(loader)).
```

Construct a [1,2,...,N] list:

```logtalk
list_constructors::flist(7, [], L).
```

<!--
L = [1, 2, 3, 4, 5, 6, 7].
-->

Construct a [N,...,2,1] list:

```logtalk
list_constructors::rlist(7, L, []).
```

<!--
L = [7, 6, 5, 4, 3, 2, 1].
-->

Sum the natural numbers 1..4:

```logtalk
list_sums::sum_first_n(4, 0, Sum).
```

<!--
Sum = 10.
-->

Sum the elements of a list:

```logtalk
list_sums::sum([2,2,3], Sum).
```

<!--
Sum = 7.
-->

Return the set of elements present in a list:

```logtalk
unique::unique([a], Unique).
```

<!--
Unique = [a].
-->

```logtalk
unique::unique([a,b,a], Unique).
```

<!--
Unique = [a,b].
-->

Compute the length of a list:

```logtalk
synopsis::len([a,b,a], Len).
```

<!--
Len = 3.
-->
