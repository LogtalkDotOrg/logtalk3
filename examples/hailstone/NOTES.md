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

# hailstone

This folder contains an implementation of the Hailstone sequence and it was
coded for a contribution to the Rosetta Stone website. For more information
see:

	http://rosettacode.org/wiki/Hailstone_sequence

	http://en.wikipedia.org/wiki/Collatz_conjecture

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(hailstone(loader)).
```

Generating a sequence:

```logtalk
hailstone::generate_sequence(10, Sequence).
```

<!--
Sequence = [10, 5, 16, 8, 4, 2, 1].
-->

```logtalk
hailstone::write_sequence(10).
```

<!--
10 5 16 8 4 2 1
true.
-->

Calculating the length of a sequence:

```logtalk
hailstone::sequence_length(27, Length).
```

<!--
Length = 112.
-->

Finding the longest sequence in an interval:
 
```logtalk
hailstone::longest_sequence(1, 100000, N, Length).
```

<!--
N = 77031, Length = 351.
-->
