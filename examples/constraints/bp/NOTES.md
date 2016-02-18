________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>

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

This folder contains a set of simple examples illustrating how to use B-Prolog
constraint support with Logtalk. These examples are adapted with permission 
from the original author, Neng-Fa Zhou.

The B-Prolog `::/2` finite-domain built-in predicate clashes with the Logtalk 
`::/2` message sending operator. The solution is to use instead the alternative
B-Prolog `in/2` built-in predicate.

The built-in `predicate_property/2` predicate fails to report some of the 
constraint predicates as built-in predicates. One workaround is to encapsulate
calls to these predicates using the `{}/1` Logtalk control construct.
