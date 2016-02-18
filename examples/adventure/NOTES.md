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


To load this example and for sample queries, please see the `SCRIPT.txt` 
file.

This folder contains Logtalk versions of some simple Prolog text
adventures:

- sleepy, written by David Matuszek, Villanova University
- spider, written by David Matuszek, Villanova University
- wumpus, written by Dan Cliburn, University of the Pacific

For the "sleepy" and "spider" adventures, I applied the necessary
changes to ensure compatibility with all the back-end Prolog compilers
that you can use with Logtalk, rewrote some of the code to avoid creation
of choice-points, and added a very simple command-line interface.

For the "wumpus" adventure, I applied the necessary changes to ensure
compatibility with all the back-end Prolog compilers that you can use
with Logtalk.

Any bugs introduced while adapting these text adventure examples to
Logtalk are solely my responsibility.

There is no support for restarting a text adventure. If you want to play
again, you must restart your Logtalk section and load the example again.
