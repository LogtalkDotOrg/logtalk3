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

# lo - travellers

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(lo_travellers(loader)).
```

Build a route by adding one town at a time:

```logtalk
incremental::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route).
```

<!--
Route = oxford-london-portsmouth-brighton-exeter-aberystwyth ;
...
-->

Presort towns by geographical distance before using the incremental algorithm:

```logtalk
presort::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route).
```

<!--
Route = brighton-london-oxford-portsmouth-exeter-aberystwyth ;
...
-->

Come home after the journey:

```logtalk
circular::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route).
```

<!--
Route = london-brighton-portsmouth-exeter-aberystwyth-oxford-london ;
...
-->

Blind search by generating permutations of the list of towns:

```logtalk
permute::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route).
```

<!--
Route = (aberystwyth-exeter-portsmouth-brighton-london-oxford,273.6237583942784).
-->
