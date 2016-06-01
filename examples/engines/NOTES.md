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

This example requires support for both threads and coroutining. Currently it
only runs on SWI-Prolog. It should run also on XSB and YAP if and when these
systems bugs with coroutining and/or threads get fixed.

This folder contains examples of using threaded engines. One of the example
meta-predicates, `metas::best_of/3`, is described in the paper:

@article{Tarau2000,
	author="Paul Tarau",
	title="Architecture and Implementation Aspects of the Lean Prolog System",
	url="http://www.cse.unt.edu/~tarau/research/LeanProlog/ArchitectureOfLeanProlog.pdf"
}
