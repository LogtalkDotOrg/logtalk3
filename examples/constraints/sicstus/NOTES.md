________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>

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

This folder contains a set of experimental examples illustrating how to use
the CLP(FD) library distributed with SICStus Prolog 4.x with Logtalk.

The CLP(FD) library is loaded from the "loader.lgt" auxiliary loader file.
This library must always be loaded prior to compilation of the individual 
example files.

In most cases, objects and categories containing CLP(FD) code must be
compiled using the hook file ("sicstus_clpfd_hook.lgt") provided in this
directory (see the "loader.lgt" file for an example). This hook file
provides support for compilation of indexicals and overrides the meta-
predicate directives of the CLP(FD) meta-predicates. When using the CLP(FD)
labeling/2 enumeration predicate, the value/1 and variable/1 options are
currently not supported.

Within objects and categories, is strongly recommended that you use the
use_module/2 directive for the CLP(FD) module.
