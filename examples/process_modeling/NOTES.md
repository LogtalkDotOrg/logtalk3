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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains an example of using parametric objects to represent
and restore shared variables between sets of constraints that are stored
in different objects. This solution is applied to process modeling where
constraints are used to represent run dependencies between processes.
Part of the process description is a set of finite domain constraints
describing how many times the process can or must be executed. An object
parameter is used to provide access to the process constraint variable.
See the comments in the `process_modeling.lgt` file for further details.

This example can be run using B-Prolog, ECLiPSe, GNU Prolog, SICStus
Prolog, SWI-Prolog, or YAP as the backend compiler (by using an ugly
solution thanks to the lack of standardization of CLP(FD) constraint
libraries).

 
