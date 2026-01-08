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

Design pattern:
	Composite

Description:
	"Compose objects into tree structures to represent part-whole
	hierarchies. Composite lets clients treat individual objects
	and compositions of objects uniformly."

This pattern can be used with both classes and prototypes.

In this sample implementation of the pattern, we choose to use classes
and object composition (i.e., composite objects will store the identifiers
of the objects that are part of the composite). In the sample queries,
we create the instances dynamically at runtime but we could also have
defined (some or all) of the instances statically in the source file.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('structural/composite/loader')).
```

First, create some ellipses:

```logtalk
ellipse::(new(e1), new(e2), new(e3), new(e4)).
```

<!--
true.
-->

Second, create some composite graphics:

```logtalk
composite_graphic::(new(cg), new(cg1), new(cg2)).
```

<!--
true.
-->

Add to the composite graphic `cg1` the ellipses `e1`, `e2`, and `e3`:

```logtalk
cg1::(add(e1), add(e2), add(e3)).
```

<!--
true.
-->

Add to the composite graphic `cg2` the ellipse `e4`:

```logtalk
cg2::add(e4).
```

<!--
true.
-->

Add to the composite graphic `cg` the composite graphics `cg1` and `cg2`:

```logtalk
cg::(add(cg1), add(cg2)).
```

<!--
true.
-->

Finally, print the contents of the top-level composite graphic `cg`:

```logtalk
cg::print.
```

<!--
ellipse: e1
ellipse: e2
ellipse: e3
ellipse: e4

true.
-->
