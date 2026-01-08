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

# module_aliases

This is a minimal example illustrating the use of the Logtalk `use_module/1`
directive to module aliases (not to be confused by the Prolog directive with
the same name). It requires backend support for modules and the `use_module/2`
directive.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Check that the Prolog backend supports modules:

```logtalk
current_logtalk_flag(modules, supported).
```

Start by loading the example:

```logtalk
logtalk_load(module_aliases(loader)).
```

Demonstration of using aliases for using or experimenting with different
modules:

```logtalk
data_source::all(Data).
```

<!--
...
Data = [1,2,3].
-->

Demonstration of using module aliases with parametric objects:

```logtalk
pardata(data1)::all(Data).
```

<!--
...
Data = [1,2,3].
-->

```logtalk
pardata(data2)::all(Data).
```

<!--
...
Data = [4,5,6]
-->
