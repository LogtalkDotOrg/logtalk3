________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

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


`linda`
=======

Linda is a classic coordination model for process communication. This library
provides a Linda tuple-space implementation. Requires both multi-threading and
sockets support. It supports SWI-Prolog, Trealla Prolog, and XVM.

The tuple-space is a shared blackboard where processes can:

- **Write** tuples using `out/1`
- **Read** tuples (without removing) using `rd/1` and `rd_noblock/1`
- **Remove** tuples using `in/1` and `in_noblock/1`

The blocking operations (`in/1`, `rd/1`) suspend the process until a matching
tuple becomes available. The non-blocking variants (`in_noblock/1`, `rd_noblock/1`)
fail immediately if no matching tuple is found.

Tuples are matched using standard Prolog unification, allowing patterns with
variables.


API documentation
-----------------

Open the [../../apis/library_index.html#linda](../../apis/library_index.html#linda)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(linda(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(linda(tester)).


Usage
-----

### Starting a server

To start a Linda server that prints its address:

	| ?- linda::linda.
	localhost:54321
	...

To start a server with a callback when it starts:

	| ?- linda::linda([(Host:Port)-format('Server at ~w:~w~n', [Host, Port])]).

The server runs until all clients disconnect after a shutdown request.

### Connecting a client

To connect to a server:

	| ?- linda::linda_client(localhost:54321).

### Tuple operations

Write a tuple:

	| ?- linda::out(message(hello, world)).

Read a tuple (blocking):

	| ?- linda::rd(message(X, Y)).
	X = hello,
	Y = world

Remove a tuple (blocking):

	| ?- linda::in(message(X, Y)).
	X = hello,
	Y = world

Non-blocking operations:

	| ?- linda::rd_noblock(message(X, Y)).
	no   % if no matching tuple exists

	| ?- linda::in_noblock(message(X, Y)).
	no   % if no matching tuple exists

Wait for one of several tuples:

	| ?- linda::in([task(1, X), task(2, X), done], Tuple).

Alternative syntax using `in_list/2`:

	| ?- linda::in_list([task(1, X), task(2, X), done], Tuple).

Similarly for reading:

	| ?- linda::rd_list([status(ready), status(waiting)], Status).

Collect all matching tuples atomically (read without removing):

	| ?- linda::findall_rd_noblock(N, counter(N), Counters).

Collect and remove all matching tuples atomically:

	| ?- linda::findall_in_noblock(X, item(X), Items).

### Disconnecting

Close the client connection:

	| ?- linda::close_client.

Request server shutdown (server stops accepting new connections):

	| ?- linda::shutdown_server,
	   linda::close_client.

### Timeout control

Set a timeout for blocking operations:

	| ?- linda::linda_timeout(Old, 5:0).  % 5 seconds timeout

Disable timeout (wait forever):

	| ?- linda::linda_timeout(_, off).


Examples
--------

### Producer-Consumer

Producer (client 1):

	producer :-
		produce(X),
		linda::out(item(X)),
		producer.

Consumer (client 2):

	consumer :-
		linda::in(item(X)),
		consume(X),
		consumer.

### Critical Region

	critical_section :-
		linda::in(mutex),  % acquire lock
		do_critical_work,
		linda::out(mutex). % release lock

Initialize the mutex:

	| ?- linda::out(mutex).

### Synchronization

Wait for a signal:

	| ?- linda::in(ready).  % blocks until someone does out(ready)

Send a signal:

	| ?- linda::out(ready).


API Summary
-----------

### Server predicates

- `linda` - Start server on automatic port
- `linda(+Options)` - Start server with options (see API documentation)

### Client predicates

- `linda_client(+Address)` - Connect to server at `Host:Port`
- `close_client` - Close connection
- `shutdown_server` - Request server shutdown
- `linda_timeout(?Old, +New)` - Get/set timeout

### Tuple operations

- `out(+Tuple)` - Add tuple to space
- `in(?Tuple)` - Remove matching tuple (blocking)
- `in_noblock(?Tuple)` - Remove matching tuple (non-blocking)
- `in(+TupleList, ?Tuple)` - Remove one of several tuples (blocking)
- `in_list(+TupleList, ?Tuple)` - Remove one of several tuples (blocking)
- `rd(?Tuple)` - Read matching tuple (blocking)
- `rd_noblock(?Tuple)` - Read matching tuple (non-blocking)
- `rd(+TupleList, ?Tuple)` - Read one of several tuples (blocking)
- `rd_list(+TupleList, ?Tuple)` - Read one of several tuples (blocking)
- `findall_rd_noblock(?Template, +Tuple, ?List)` - Collect all matching tuples (atomic read)
- `findall_in_noblock(?Template, +Tuple, ?List)` - Collect and remove all matching tuples (atomic remove)


Known issues
------------

Recent versions of macOS seem to disable the mapping of `localhost` to
`127.0.0.1`. This issue may prevent some functionality from working. This
can be fixed either by editing the `/etc/hosts` file or by using
`'127.0.0.1'` as the host argument instead of `localhost`.


See also
--------

- SICStus Prolog Linda documentation:
  https://sicstus.sics.se/sicstus/docs/latest4/html/sicstus.html/lib_002dlinda.html

- Original Linda papers:
  - Carreiro, N. & Gelernter, D. (1989). Linda in Context.
  - Carreiro, N. & Gelernter, D. (1989). How to Write Parallel Programs: A Guide to the Perplexed.
