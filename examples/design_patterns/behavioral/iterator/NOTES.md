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

Design pattern:
	Iterator

Description:
	"Provide a way to access the elements of an aggregate object
	sequentially without exposing its underlying representation."

This pattern can be used with both classes and prototypes.

A straightforward implementation of this design pattern is to define an
iterator predicate that enumerates elements using backtracking. This
predicate can then be called from meta-predicates such as the de facto
standard `forall/2` predicate.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('behavioral/iterator/loader')).
```

Add some books to our collection:

```logtalk
book_collection::(
		add_title('Design Patterns'),
		add_title('Design Patterns in Dynamic Languages'),
		add_title('Analysis patterns')
     ).
```

<!--
true.
-->

Use the iterator predicate to print all titles in the collection:

```logtalk
forall(book_collection::element(Title), (write(Title), nl)).
```

<!--
Design Patterns
Design Patterns in Dynamic Languages
Analysis patterns

true.
-->
