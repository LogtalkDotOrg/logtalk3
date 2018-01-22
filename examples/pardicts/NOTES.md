________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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

This example requires using SWI-Prolog 7.x as the backend Prolog compiler.
It illustrates using a SWI-Prolog native dictionary term for representing
a parametric object parameters. This is accomplished by using a single
parameter, instantiated to a dictionary, and by defining a set of predicates
for accessing the individual parameters by a key (instead of using the
Logtalk built-in `parameter/2` method that indexes individual parameters by
position). The access predicates are goal-expanded to the corresponding
SWI-Prolog dictionary built-in predicates.
