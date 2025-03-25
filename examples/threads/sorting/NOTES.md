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

# threads - sorting

This folder contains a multi-threading implementation of the merge sort
algorithm.  Depending on the size of the lists that are ordered, using
only one thread can be faster. The number of threads to be use in sorting
is set using the `qsort/1` and `msort/1` object parameters. You may need
to adjust the size of the memory areas used by your Prolog compiler,
depending on the size of the lists you want to sort.

This example uses a simple implementation of the merge sort algorithm,
intended only to illustrate Logtalk multi-threading features. For any
other purpose, you may find the following paper a worthwhile reading:

@incollection{ apt93modular,
	author = "Krzysztof R. Apt and Dino Pedreschi",
	title = "Modular Termination Proofs for Logic and Pure Prolog Programs.",
	booktitle = "116",
	month = "31",
	publisher = "Centrum voor Wiskunde en Informatica (CWI)",
	address = "ISSN 0169-118X",
	pages = "35",
	year = "1993",
	url = "citeseer.ist.psu.edu/apt93modular.html"
}

You probably want to play with the list sizes in order to find out when the
lists to be sorted are big enough to make the use of multi-threading worth
performance-wise (i.e., to compensate the overhead of thread creation and
management).

The implementation of the Quicksort algorithm makes the possible performance
gains due to the use of multi-threading highly dependent on the pivots used
for vector partition. Increasing the number of threads alleviates the problem
provided an adequate number of processing cores.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(sorting(loader)).
```

NOTE: some example queries below use a proprietary predicate `time/1` in
order to get accurate goal times. This predicate is found on several Prolog
systems. For other Prolog compilers, replace the `time/1` call by any
appropriate timing calls (e.g., `cputime/0`).

Generate a big list of random floats and then merge sort it using a single thread:

```logtalk
generator::list(20000, List), time(msort(1)::msort(List, Sorted)).
```

<!--
% 1,145,746 inferences, 0.40 CPU in 0.43 seconds (93% CPU, 2864365 Lips)

List = [0.326219, 0.545052, 0.21687, 0.0500493, 0.772745, 0.805005, 0.574483, 0.301708, 0.670021|...],
Sorted = [1.39358e-06, 0.000206126, 0.00026088, 0.000299165, 0.000362691, 0.000397709, 0.000539889, 0.000574419, 0.000578717|...] 

true.
-->

Generate a big list of random floats and then merge sort it using two threads:

```logtalk
generator::list(20000, List), time(msort(2)::msort(List, Sorted)).
```

<!--
% 80,067 inferences, 0.32 CPU in 0.21 seconds (150% CPU, 250209 Lips)

List = [0.963245, 0.666814, 0.3841, 0.281952, 0.806571, 0.608224, 0.623344, 0.138888, 0.867367|...],
Sorted = [5.89827e-05, 0.00010463, 0.000105771, 0.000171936, 0.00022632, 0.000378509, 0.000392918, 0.00041885, 0.000482844|...] 

true.
-->

Generate a big list of random floats and then merge sort it using four threads:

```logtalk
generator::list(20000, List), time(msort(4)::msort(List, Sorted)).
```

<!--
% 80,079 inferences, 0.32 CPU in 0.16 seconds (204% CPU, 250247 Lips)

List = [0.0923009, 0.443585, 0.72304, 0.945816, 0.501491, 0.311327, 0.597448, 0.915656, 0.666957|...],
Sorted = [3.65916e-05, 4.06822e-05, 5.07434e-05, 6.09007e-05, 0.000134275, 0.000190491, 0.00024128, 0.000361441, 0.000412926|...] 

true.
-->

Generate a big list of random floats and then quick sort it using a single thread:

```logtalk
generator::list(20000, List), time(qsort(1)::qsort(List, Sorted)).
```

<!--
% 1,145,746 inferences, 0.40 CPU in 0.43 seconds (93% CPU, 2864365 Lips)

List = [0.326219, 0.545052, 0.21687, 0.0500493, 0.772745, 0.805005, 0.574483, 0.301708, 0.670021|...],
Sorted = [1.39358e-06, 0.000206126, 0.00026088, 0.000299165, 0.000362691, 0.000397709, 0.000539889, 0.000574419, 0.000578717|...] 

true.
-->

Generate a big list of random floats and then quick sort it using two threads:

```logtalk
generator::list(20000, List), time(qsort(2)::qsort(List, Sorted)).
```

<!--
% 80,067 inferences, 0.32 CPU in 0.21 seconds (150% CPU, 250209 Lips)

List = [0.963245, 0.666814, 0.3841, 0.281952, 0.806571, 0.608224, 0.623344, 0.138888, 0.867367|...],
Sorted = [5.89827e-05, 0.00010463, 0.000105771, 0.000171936, 0.00022632, 0.000378509, 0.000392918, 0.00041885, 0.000482844|...] 

true.
-->

Generate a big list of random floats and then quick sort it using four threads:

```logtalk
generator::list(20000, List), time(qsort(4)::qsort(List, Sorted)).
```

<!--
% 80,079 inferences, 0.32 CPU in 0.16 seconds (204% CPU, 250247 Lips)

List = [0.0923009, 0.443585, 0.72304, 0.945816, 0.501491, 0.311327, 0.597448, 0.915656, 0.666957|...],
Sorted = [3.65916e-05, 4.06822e-05, 5.07434e-05, 6.09007e-05, 0.000134275, 0.000190491, 0.00024128, 0.000361441, 0.000412926|...] 

true.
-->
