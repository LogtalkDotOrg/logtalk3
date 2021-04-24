________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>  
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

This example illustrates the predicate declaration and predicate definition
lookup algorithms used when sending a message to an object. For full details,
see the Handbook section on inheritance.

The lookup algorithms differ for instances and for prototypes and also depend
if the lookup is for a predicate declaration or for a predicate definition.
Sending a message to an object, requires two predicate lookups (performed at
compile time, when possible):

- Lookup the predicate declaration to check that the predicate in within
the scope of the *sender*. In the most common cases, this means that the
predicate is declared public.

- Assuming that the predicate exists and is within scope, lookup the
predicate definition to answer the message. If none found, the message
simply fails as per the Closed World Assumption (CWA).

See the comments in the `prototypes.lgt` and `classes.lgt` source files
and the sample queries in the `SCRIPT.txt` file for further details.
