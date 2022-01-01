________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
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


`redis`
=======

Redis client library. Supports GNU Prolog, LVM, Qu-Prolog, SICStus Prolog,
SWI-Prolog, and XSB. Support for Ciao Prolog and ECLiPSe is also included
but requires fixes for issues in these systems.

For general information on Redis, including a list of the available
commands, visit:

	https://redis.io


API documentation
-----------------

Open the [../../docs/library_index.html#redis](../../docs/library_index.html#redis)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(redis(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(redis(tester)).

The tests assume a localhost Redis server running on the default port (6379)
if the `REDIS_HOST` and `REDIS_PORT` environment variables are not defined.
If the server is not detected, the tests are skipped.

The unit tests were originally written by Sean Charles for his GNU Prolog
Redis client library:

	https://github.com/emacstheviking/gnuprolog-redisclient

The Logtalk version is a straight-forward port of the original tests using
the `test/1` dialect of `lgtunit`.


Credits
-------

This library is inspired by the Sean Charles GNU Prolog Redis client library.


Known issues
------------

Recent version of macOS seem to disable the mapping of `localhost` to
`127.0.0.1`. This issue may prevent running this library unit tests
and the `redis::connect/1` from working. This can be fixed either by
editing the `/etc/hosts file or by using in alternative the predicate
`redis::connect/3` with `'127.0.0.1'` as first argument.


