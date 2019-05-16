________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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


About
-----

This library provides a dictionary (also know as associative array, map,
or symbol table) protocol, a simple binary tree implementation, and a
red–black tree implementation.


API documentation
-----------------

Open the [../docs/index.html](../docs/index.html) file in a web browser
and choose the library index.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(dictionaries(loader)).


Credits
-------

The red-black tree implementation is an adaptation to Logtalk of the `rbtrees`
Prolog library authored by Vitor Santos Costa.
