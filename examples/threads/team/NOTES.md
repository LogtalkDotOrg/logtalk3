________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
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
