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
