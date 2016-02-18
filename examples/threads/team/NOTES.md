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

This folder contains an implementation of a synchronous concurrency task
and it was coded for a contribution to the Rosetta Code website:

	"One of the concurrent units will read from a file named "input.txt"
	and send the contents of that file, one line at a time, to the other
	concurrent unit, which will print the line it receives to standard output.
	The printing unit must count the number of lines it prints. After the 
	concurrent unit reading the file sends its last line to the printing unit,
	the reading unit will request the number of lines printed by the printing
	unit. The reading unit will then print the number of lines printed by the
	printing unit. This task requires two-way communication between the concurrent
	units. All concurrent units must cleanly terminate at the end of the program."

For more information see:

	http://rosettacode.org/wiki/Synchronous_concurrency
