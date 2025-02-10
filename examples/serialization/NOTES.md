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

# serialization

This example illustrates a simple solution for serializing dynamic objects
that conform to a given protocol. The serialization data is saved to a file.
Restoring this file recreates the serialized objects. Object identities are
not perserved however. This example assumes that the dynamic objects contain
only facts. See also the `persistency` example.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(serialization(loader)).
```

<!--
true.
-->

Create a protocol declaring some predicates:

```logtalk
create_protocol(abc, [], [public([a/1,b/1,c/1])]).
```

<!--
true.
-->

Create some dynamic objects implementing the protocol:

```logtalk
create_object(Object1, [implements(abc)], [], [a(1),b(1),c(1)]),
create_object(Object2, [implements(abc)], [], [a(2),b(2),c(2)]),
create_object(Object3, [implements(abc)], [], [a(3),b(3),c(3)]).
```

<!--
Object1 = o1, Object2 = o2 Object3 = o3.
-->

Save the objects to a file:

```logtalk
serializer::save(abc, abc_objects).
```

<!--
true.
-->

Abolish all objects:

```logtalk
forall(conforms_to_protocol(Object,abc), abolish_object(Object)).
```

<!--
true.
-->

Restore the serialized objects from the file:

```logtalk
serializer::restore(abc_objects).
```

<!--
true.
-->

Confirm the restoring process worked as expected:

```logtalk
%%table
conforms_to_protocol(Object, abc), Object::(a(A), b(B), c(C)).
```

<!--
Object = o3, A = B, B = C, C = 1 ;
Object = o4, A = B, B = C, C = 2 ;
Object = o5, A = B, B = C, C = 3.
-->
