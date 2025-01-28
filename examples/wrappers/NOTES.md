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

# wrappers

This example illustrates how to use the `begin_of_file` term generated
when compiling a source file to create an object wrapper for the code
in a plain Prolog file. Assuming that the `context_switching_calls` is
set to `allow`, the generated object predicates can be called using the
`(<<)/2` debugging control construct for testing. This wrapper is useful
for e.g. using the Logtalk compiler lint checks to examine predicate
call dependencies of the wrapped code and also to look for possible
portability issues when the `portability` flag is set to `warning`.

Start by loading the hook object:

```logtalk
logtalk_load(wrappers(wrapper)).
```

Compile the `zipper.pl` plain Prolog source file using the hook object:

```logtalk
logtalk_load('zipper.pl', [hook(wrapper)]).
```

Try one of the generated "zipper" object predicates:

```logtalk
zipper<<(zipper(3, [1,2,3,4,5], Zip, X), next(Zip, Next)).
```

<!--
Zip = zip([2, 1], 3, [4, 5]), X = 3, Next = zip([3, 2, 1], 4, [5]).
-->
