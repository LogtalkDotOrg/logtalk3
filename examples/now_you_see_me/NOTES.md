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

# now_you_see_me

This example illustrates that the implementation of dynamic predicates must
ensure that retracting all local clauses for an inherited dynamic predicate
restores the visibility of any inherited definition. This also have direct
consequences for the implementation and optimization of `(^^)/1` calls. For
more information on this example, please see the comments in the example
source files. The example is inspired by the "Now You See Me" movie, whose
main characters are four stage magicians known as the "Four Horsemen".

Start by loading the example:

```logtalk
logtalk_load(now_you_see_me(loader)).
```

Show all the horseman on stage

```logtalk
stage::list.
```

<!--
danny
merritt
henley
jack

true.
-->

Hide the four horseman:

```logtalk
magic::hide.
```

<!--
true.
-->

```logtalk
stage::list.
```

<!--
true.
-->

Again show all the horseman on stage:

```logtalk
magic::show.
```

<!--
true.
-->

```logtalk
stage::list.
```

<!--
danny
merritt
henley
jack

true.
-->
