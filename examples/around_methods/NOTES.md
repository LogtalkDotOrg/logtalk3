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

# around_methods

This example illustrates how to use a complementing category to define what
is often described as an "around method". It uses the **experimental** `@/1`
goal annotation that allows calling the original predicate definition (in
the patched object) from its new definition in the category. This annotation
allows the new definition to call some goals, followed by a call to the
original definition, followed by calls to some more goals, hence the name
"around method".

Start by loading the unpatched object:

```logtalk
logtalk_load(around_methods(bird)).
```

Call the unpatched bird::make_sound/0 predicate:

```logtalk
bird::make_sound.
```

<!--
Chirp, chirp!
yes
-->

Next load the complementing category defining the patch for the bird::make_sound/0 predicate:

```logtalk
logtalk_load(around_methods(patch)).
```

Call the now patched bird::make_sound/0 predicate:

```logtalk
bird::make_sound.
```

<!--
Started making sound...
Chirp, chirp!
... finished making sound.
yes
-->
