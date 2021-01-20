________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

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


`term_io`
=========

This library implements predicates for reading/writing terms from/to atoms,
chars (lists of characters), and codes (lists of character codes). These
predicates are implemented using a single temporary file created when the
library is loaded. This temporary file is unique per Logtalk process. The
predicates can be safely used in multi-threaded applications.


API documentation
-----------------

Open the [../../docs/library_index.html#term_io](../../docs/library_index.html#term_io)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(term_io(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(term_io(tester)).
