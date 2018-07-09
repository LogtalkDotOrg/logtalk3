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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This example illustrates that the implementation of dynamic predicates must
ensure that retracting all local clauses for an inherited dynamic predicate
restores the visibility of any inherited definition. This also have direct
consequences for the implementation and optimization of `^^/1` calls. For
more information on this example, please see the comments in the example
source files. The example is inspired by the "Now You See Me" movie, whose
main characters are four stage magicians known as the "Four Horsemen".
