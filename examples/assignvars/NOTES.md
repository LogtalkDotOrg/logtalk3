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

This example illustrates the use of assignable variables and parametric 
objects as alternative implementation to dynamic object predicates for
storing (backtrackable) object state. For more information on assignable 
variables please consult the URL:

	http://www.kprolog.com/en/logical_assignment/

The objects in this example make use of the library category `assignvars`.
This category contains an adaptation of the pure logical subset implementation
of assignable variables by Nobukuni Kino, which can be found on the URL above.
