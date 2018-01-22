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


This folder contains some examples of multi-threading programming.
Multi-threading programming is only supported on some Prolog compilers.
Currently this includes SWI-Prolog, YAP CVS, and XSB CVS (make sure that 
you use the multi-threading versions of these Prolog compilers!). Moreover, 
multi-threading may be turned off by default. In order to run the examples, 
you may need to first turn on multi-threading support on the Prolog adapter 
files.

Some of the examples try to benchmark single-threaded and multi-threaded 
solutions. Depending on the Prolog compiler, the operating-system, and the 
computer used, you may need to adjust the size of the problem data in order 
to find the threshold where multi-threading solutions begin to outperform 
the single-threaded solutions.

Some of the examples may imply adjusting the default size of thread data 
areas or, preferably, use of the 64 bits version of the compatible Prolog 
compilers.

There are known Prolog bugs on the multi-threading support found on XSB, 
YAP, and SWI-Prolog. These bugs prevent some examples to run and may lead 
to crashes. Some of the bugs are platform-specific, only occurring on some 
operating-systems.
