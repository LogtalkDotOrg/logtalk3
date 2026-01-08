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

# complements (restrict)

This folder contains an example that show how to use a category to
explicitly complement an existing object compiled with the `complements`
flag set to `restrict`. With this setting, a category can only add new
functionality to the complemented object.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the non-patched vault objects:

```logtalk
logtalk_load(complements_restrict(vaults)).
```

Open the `my_vault` vault using the correct password:

```logtalk
my_vault::open('!"#$%&/()=').
```

Any other password will be rejected. For example:

```logtalk
my_vault::open('abc123').
```

<!--
false.
-->

Now load the `hacker` complementing category:

```logtalk
logtalk_load(complements_restrict(hacker)).
```

Try the hacker replaced password and fail miserably:

```logtalk
my_vault::open('1234567890').
```

<!--
You have been hacked by SmartPants!
false.
-->

The correct, original, password is still the only one capable
of opening the vault, despite the hacker messages:

```logtalk
my_vault::open('!"#$%&/()=').
```

<!--
You have been hacked by SmartPants!
true.
-->
