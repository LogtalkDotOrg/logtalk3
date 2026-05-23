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


`http_cookies`
==============

This library implements validation, parsing, and generation of HTTP `Cookie`
and `Set-Cookie` header field values following RFC 6265 syntax. Header values
can be represented as atoms, character lists, or code lists.

The library predicates are defined in the `http_cookies(_Representation_)`
parametric object where `_Representation_` can be one of:

- `atom` - cookie texts are represented as atoms
- `chars` - cookie texts are represented as lists of characters
- `codes` - cookie texts are represented as lists of character codes

The parameter must be bound when sending messages to the object.

`Cookie` header values are represented as lists of `Name-Value` pairs.

`Set-Cookie` header values are represented by a cookie name, a cookie value,
and a list of attributes.

where `Attributes` use `Key-Value` notation and can contain:

- `expires-Date`
- `max_age-Seconds`
- `domain-Domain`
- `path-Path`
- `secure-true`
- `http_only-true`
- `extension-Attribute`

Unknown Set-Cookie attributes are preserved as `extension-Attribute` pairs so
long as they do not reuse one of the reserved RFC 6265 attribute names.


API documentation
-----------------

Open the [../../apis/library_index.html#http_cookies](../../apis/library_index.html#http_cookies)
link in a web browser.

Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(http_cookies(loader)).

Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(http_cookies(tester)).
