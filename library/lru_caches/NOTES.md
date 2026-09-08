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


`lru_caches`
============

This library implements immutable LRU (Least Recently Used) caches. Cache
representations should be regarded as opaque terms and only accessed using
the library predicates.


API documentation
-----------------

Open the [../../apis/library_index.html#lru-caches](../../apis/library_index.html#lru-caches)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(lru_caches(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(lru_caches(tester)).


Usage
-----

Create an empty cache by specifying its maximum number of entries:

	| ?- lru_cache::new(2, Cache).
	Cache = ...
	yes

Cache operations return new cache terms, leaving their input caches
unchanged. The `put/4` predicate inserts or replaces an entry and marks it as
the most recently used entry. When inserting into a full cache, the least
recently used entry is evicted:

	| ?- lru_cache::(
			new(2, Cache0),
			put(a, 1, Cache0, Cache1),
			put(b, 2, Cache1, Cache2),
			put(c, 3, Cache2, Cache3),
			as_list(Cache3, Pairs)
		).
	Pairs = [c-3,b-2]
	yes

The `lookup/3` predicate reads an entry without changing its recency. The
`get/4` predicate instead returns an updated cache where the matching entry
is the most recently used. The `as_list/2` predicate returns entries ordered
from most recently used to least recently used.

A cache with capacity zero remains empty and `put/4` is a no-op. Cache keys
must be ground terms; values may be arbitrary terms.

The implementation uses separate balanced-tree indexes for keys and
recency. The `capacity/2`, `size/2`, `empty/1`, and `clear/2` predicates take
constant time. Lookup and mutation predicates take logarithmic time, and
`as_list/2` takes linear time.
