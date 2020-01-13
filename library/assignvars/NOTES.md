________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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


About
-----

The `assignvarsp` protocol declares the predicates used for logical assignment 
of Prolog terms developed by Nobukuni Kino.

The `assignvars` object provides a declarative implementation of the `assignvarsp`
protocol. It can be used with any backend Prolog compiler. 

The `nd_assignvars` object provides a non-declarative but faster implementation
of the `assignvarsp` protocol. It can be used with the following backend Prolog
compilers: B-Prolog, CxProlog, ECLiPSe, GNU Prolog, Qu-Prolog, SICStus Prolog,
SWI-Prolog, and YAP.

For more information on assignvars, please consult the URL:

	http://www.kprolog.com/en/logical_assignment/


API documentation
-----------------

Open the [../../docs/library_index.html#assignvars](../../docs/library_index.html#assignvars)
file in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(assignvars(loader)).
