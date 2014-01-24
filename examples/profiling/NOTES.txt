________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This is a very simple example of the use of events and monitors to make 
profilers for an application. It's easy to modify to make it do much more. 
For instance, most Prolog compilers give you access to data concerning
space usage (stacks, heap, etc).

The example defines three objects:

- `message_counter`  
	using events, this object allows us to count the messages sent to
	spied objects 

- `stop_watch`  
	using events, this object simply prints the CPU time before and
	after a message sent to a spied object

- `timer`  
	this object implements a method that sends a message to an object
	a specified number of times, returning the average execution time
