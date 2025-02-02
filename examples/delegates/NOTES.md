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

# delegates

This folder contains an implementation of the delegation design pattern and
is based on the sample code found on the Rosetta Code website. For more
information see:

http://en.wikipedia.org/wiki/Delegation_pattern

http://rosettacode.org/wiki/Delegates#Logtalk

Load the example:

```logtalk
logtalk_load(delegates(loader)).
```

Without a delegate:

```logtalk
a_delegator::operation(String).
```

<!--
String = 'default implementation'.
-->

With a delegate that does not implement `thing/1`:

```logtalk
a_delegator::set_delegate(an_object), a_delegator::operation(String).
```

<!--
String = 'default implementation'.
-->

With a delegate that implements `thing/1`:

```logtalk
a_delegator::set_delegate(a_delegate), a_delegator::operation(String).
```

<!--
String = 'delegate implementation'.
-->

Same queries but using the parametric object implementation:

```logtalk
a_delegator(an_object)::operation(String).
```

<!--
String = 'default implementation'.
-->

```logtalk
a_delegator(a_delegate)::operation(String).
```

<!--
String = 'delegate implementation'
-->
