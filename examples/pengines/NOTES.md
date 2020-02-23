________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 2020 Michael T. Richter and Paulo Moura <pmoura@logtalk.org>

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

This example illustrates how to use SWI-Prolog Pengines from objects. It
is based on simple example provided in the Pengines documentation:

https://www.swi-prolog.org/pldoc/man?section=pengine-examples

The main issue when reusing the original example code from within an object
is that the `pengines:pengine_create/1` meta-predicate template is ambiguous
due to the use of `:` as the meta-predicate argument specifier (Logtalk is
not based on a predicate-prefixing mechanism as used by Prolog modules).
Thus, we must override the template using the following directive to avoid
a compilation error:

	:- meta_predicate(pengines:pengine_create(*)).

Two object versions are provided. The first version, `dumper`, uses the
original example code plus the overriding directive above to write all
the pengine answers to the current output.

The second version, `engines`, uses a threaded engine to provide an interface
to the pengine in order to access the answers on demand with (1) separate
predicates for creating the pengine and for querying its answers (including
easily collecting all query answers in a list) and (2) asking the pengine to
start computing the next solution when the current solution is retrieved.

The minimal `pengine_server` Prolog module code is also based on the pengines
documentation available at:

https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pengines.html%27)
