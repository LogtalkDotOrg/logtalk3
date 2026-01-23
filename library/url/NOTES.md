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


`url`
=====

This library implements validation, parsing, generating, and normalization
of URLs, which can be represented as atoms, character lists, or code lists.
It currently supports the following URL schemes:

Web protocols:

- `http`
- `https`
- `ws`
- `wss`
- `gopher`

File transfer and version control:

- `ftp`
- `ftps`
- `sftp`
- `git`

File access:

- `file`

Databases:

- `jdbc`
- `mongodb`
- `mysql`
- `postgresql`

Email and news:

- `mailto`
- `news`
- `nntp`

Media streaming:

- `mms`
- `rtmp`
- `rtsp`

Shell access:

- `ssh`
- `telnet`

Directory services:

- `ldap`
- `ldaps`

Other protocols:

- `tel`
- `urn`

The library predicates are defined in the `url(_Representation_)` parametric
object where `_Representation_` can be one of:

- `atom` - strings are represented as atoms
- `chars` - strings are represented as lists of characters
- `codes` - strings are represented as lists of character codes

The parameter must be bound when sending messages to the object.


API documentation
-----------------

Open the [../../apis/library_index.html#url](../../apis/library_index.html#url)
link in a web browser.

Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(url(loader)).

Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(url(tester)).
