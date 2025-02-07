---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
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

# engines - interactors

This example requires support for both threads and coroutining. Currently it
runs on ECLiPSe and SWI-Prolog. It should run also on XSB and YAP if and when
these systems bugs with coroutining and/or threads get fixed.

This folder contains examples of interactors implemented using threaded engines.
Interactors are described in the paper:

@article{Tarau2000,
	author="Paul Tarau",
	title="Architecture and Implementation Aspects of the Lean Prolog System",
	url="http://www.cse.unt.edu/~tarau/research/LeanProlog/ArchitectureOfLeanProlog.pdf"
}

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(interactors(loader)).
```

<!--
true.
-->

Get natural numbers in increasing order:

```logtalk
interactors::natural(N).
```

<!--
N = 1.
-->

```logtalk
interactors::natural(N).
```

<!--
N = 2.
-->

```logtalk
interactors::natural(N).
```

<!--
N = 3.
-->

Get prime numbers in increasing order:

```logtalk
interactors::prime(P).
```

<!--
N = 2.
-->

```logtalk
interactors::prime(P).
```

<!--
N = 3.
-->

```logtalk
interactors::prime(P).
```

<!--
N = 5.
-->

Interactor with goal injection:

```logtalk
interactors<<sums(S).
```

<!--
S =  (0->2).
-->

```logtalk
interactors<<sums(S).
```

<!--
S =  (2->7).
-->

```logtalk
interactors<<sums(S).
```

<!--
S =  (7->9).
-->
