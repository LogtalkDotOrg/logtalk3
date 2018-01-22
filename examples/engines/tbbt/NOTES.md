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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This example implements the rock, paper, scissors, lizard, Spock game played
in the "The Big Bang Theory" sitcom using one threaded engine per player. See:

	http://www.samkass.com/theories/RPSSL.html

Currently this example requires SWI-Prolog. It should run also on XSB and YAP
if and when these systems fix the bugs in their multi-threading support.
