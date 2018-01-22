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


To example contains only unit tests for the Redis client library but
these tests also serve as examples of using this library. To run all
the unit tests, please see the `SCRIPT.txt` file. The tests assume a
local Redis server running on the default port. If the server is not
detected, the tests are skipped.

The unit tests were originally written by Sean Charles for his GNU Prolog
Redis client library:

	https://github.com/emacstheviking/gnuprolog-redisclient

The Logtalk version is a straight-forward port of the original tests using
the `test/1` dialect of `lgtunit`.
