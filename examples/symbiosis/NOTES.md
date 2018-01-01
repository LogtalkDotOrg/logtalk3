________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
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

This folder contains several examples of using Prolog built-in meta-predicates
and module meta-predicates that take closures as arguments. It is used to help
testing Logtalk support for dealing with closures as it requires generation of
a helper predicate per call to workaround a clash between the way Logtalk
compiles predicates and the way a closure is extended to form a goal.

This example supports using ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog,
and YAP as the backend compilers.
