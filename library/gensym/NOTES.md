________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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


`gensym`
========

The `gensym` library implements predicates for generating unique atoms. The
public predicates are declared synchronized to prevent race conditions when
using a backend Prolog compiler with multi-threading support.


API documentation
-----------------

Open the [../../docs/library_index.html#gensym](../../docs/library_index.html#gensym)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(gensym(loader)).
