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

# metaclasses

This example illustrates how to work with metaclasses in Logtalk.
See also the `reflection` and `roots` examples for a followup on
using metaclasses.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(metaclasses(loader)).
```

Play and modify the example code to get comfortable using classes and
instances in Logtalk.

```logtalk
root::new(I).
```

<!--
Instance created.
I = o1.
-->

```logtalk
subclass1::new(I).
```

<!--
Instance created.
Instance initialized.
I = o2.
-->

```logtalk
subclass2::new(I).
```

<!--
I = o3.
-->
