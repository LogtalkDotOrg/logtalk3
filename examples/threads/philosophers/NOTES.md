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

This folder contains a Logtalk implementation of the classical "dining 
philosophers" multi-threading problem.

For more information, consult e.g. the following URL:

	http://en.wikipedia.org/wiki/Dining_philosophers_problem

Two different implementations are provided, both using the same solution for 
avoiding deadlock (which is having one philosopher picking its chopsticks 
in a different order from the other philosophers; see the URL above for 
details): one implementations uses a category and five philosopher objects 
while the second implementation uses a parametric object.
