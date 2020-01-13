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


About
-----

The objects `event_registry`, `before_event_registry`, and `after_event_registry`
implement convenient predicates for registering before and after events.

The code makes use of the `monitoring` built-in protocol, which declares the two
basic event handler predicates (`before/3` and `after/3`). You will need to refer
to this protocol in your objects if you want to use the super control structure
`(^^/1)` with these predicates.

The `monitor` object implements more sophisticated event handling predicates.


API documentation
-----------------

Open the [../../docs/library_index.html#events](../../docs/library_index.html#events)
file in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` loader file:

	| ?- logtalk_load(events(loader)).
