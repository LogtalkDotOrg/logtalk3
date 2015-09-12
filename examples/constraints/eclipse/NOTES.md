________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>

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

This folder contains a set of simple examples illustrating how to use the IC 
Constraint Solver library distributed with ECLiPSe with Logtalk.

These examples are adapted with permission from the examples found at:

	http://www.eclipse-clp.org/examples

The examples code was changed to avoid using ECLiPSe special features (e.g. 
array notation or the do/2 loop operator) as they do not work when the "iso" 
library is loaded (this library is loaded by the "eclipse*iso.pl" config 
file, which is used in the Logtalk integration scripts and shortcuts).

The Constraint Solver libraries are loaded from the "loader.lgt" auxiliary 
loader file. These libraries must always be loaded prior to compilation of 
the individual example files.

We must define an alias for the ECLiPSe "ic" library operator ::/2 in order 
to avoid conflicts with the ::/2 Logtalk message sending operator. In the 
examples, the operator ins/2 was chosen as the alias for the ::/2 operator.
ECLiPSE 6.0#78 adds an alias in_set_range/2 for ::/2 that could also be used.
