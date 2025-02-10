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

# slides

This is a simple example of using the library list zipper implementation
to implement a basic slideshow functionality allowing remote commands to
navigate to the next, previous, first, and last slide in a sequence of
slides. The slides are represented using a predicate with two arguments,
the index and the slide contents. The main predicate, `show/2` takes a
sequence of slides and the name of the predicate, which is used as a
closure for displaying the slides.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(slides(loader)).
```

Start a slideshow; the remote accepts as input the following single
characters: n (next), p (previous), f (first), l (last), e (end)
(skip if running as a notebook):

```logtalk
(current_object(jupyter) -> true; slides::show([1,2,3,4,5,6], slide)).
```

<!--
First slide
remote: n.
Second slide
remote: n.
Third slide
remote: n.
Fourth slide
remote: p.
Third slide
remote: f.
First slide
remote: l.
Last slide
remote: e.

true.
-->
