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

# modules

This example illustrates compilation of Prolog module files as objects.

Due to the lack of standardization of Prolog module systems, the module 
files can only a common subset of Prolog module directives. Consult the
"Prolog Integration and Migration Guide" in the Logtalk documentation 
for details.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by compiling and loading the Prolog module files as objects:

```logtalk
logtalk_load(modules(loader)).
```

Call one of the module exported predicates using message-sending:

```logtalk
test::names.
```

<!--
paulo
carlos
helena

true.
-->

Same goal as above but the call is made using a meta-predicate
imported from other module:

```logtalk
test::test.
```

<!--
paulo
carlos
helena

true.
-->

Test the compilation of the module `export/1` directive; module 
exported predicates become public predicates:

```logtalk
exports::current_predicate(Pred).
```

<!--
Pred = p/1.
-->

List the properties of the `exports` public predicate `p/1`:

```logtalk
%%table
exports::predicate_property(p(_), Prop).
```

<!--
Prop = public ;
Prop = static ;
Prop = declared_in(exports) ;
Prop = defined_in(exports).
-->

Call the module exported/public predicate using message-sending:

```logtalk
%%table
exports::p(N).
```

<!--
N = 1 ;
N = 2 ;
N = 3.
-->

Test the compilation and use of meta-predicates:

```logtalk
test::names(Names).
```

<!--
Names == [paulo, carlos, helena].
-->

```logtalk
test::test(Names).
```

<!--
Names == [paulo, carlos, helena].
-->
