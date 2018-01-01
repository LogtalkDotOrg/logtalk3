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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This example requires support for both threads and coroutining. Currently it
only runs on SWI-Prolog. It should run also on XSB and YAP if and when these
systems bugs with coroutining and/or threads get fixed.

This folder contains examples of fluents implemented using threaded engines.
Fluents are described in the paper:

@inbook{Tarau2000,
	author="Tarau, Paul",
	editor="Lloyd, John and Dahl, Veronica and Furbach, Ulrich and Kerber, Manfred and Lau, Kung-Kiu and Palamidessi, Catuscia and Pereira, Lu{\'i}s Moniz and Sagiv, Yehoshua and Stuckey, Peter J.",
	chapter="Fluents: A Refactoring of Prolog for Uniform Reflection and Interoperation with External Objects",
	title="Computational Logic --- CL 2000: First International Conference London, UK, July 24--28, 2000 Proceedings",
	year="2000",
	publisher="Springer Berlin Heidelberg",
	address="Berlin, Heidelberg",
	pages="1225--1239",
	isbn="978-3-540-44957-7",
	doi="10.1007/3-540-44957-4_82",
	url="http://dx.doi.org/10.1007/3-540-44957-4_82"
}
