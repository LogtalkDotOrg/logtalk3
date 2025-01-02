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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This example illustrates the difference between *declaring* a predicate and
*defining* a predicate and the Closed World Assumption (CWA) semantics as
implemented in Logtalk when calling predicates and sending messages.

The CWA states that:

- What is true is known to be true (i.e., what is true can be proved).
- What is not know to be true, is false (i.e., what cannot be proved true,
is false).

When applied to Logtalk (or Prolog), proofs are constructed using predicate
definitions. But Logtalk also provides a clear distinction between declaring
a predicate and defining a predicate (1). This distinction is translates to
the following refined CWA semantics:

- Messages or calls for declared but undefined predicates fail.
- Messages or calls for unknown (not declared) predicates throw an error.

The Logtalk linter reports whenever possible the cases that will likely
result in runtime error.

(1) This is a necessary requirement for protocols/interfaces: we must be
able to declare a predicate without necessarily defining it.
