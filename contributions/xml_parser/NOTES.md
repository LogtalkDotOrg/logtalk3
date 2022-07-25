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


`xml_parser`
============

This folder contains a Logtalk version of John Fletcher's Prolog XML 
parser:

https://binding-time.co.uk/index.php/Parsing_XML_with_Prolog

For a detailed description of this XML parser, please see the comments 
in the `xml.lgt` source file or convert the automatically generated 
documentation to HTML or PDF. For sample queries, please see the `SCRIPT.txt`
file.

See the copyright and license information on the contributed files for 
usage and distributions conditions.


API documentation
-----------------

Open the [../../docs/library_index.html#xml_parser](../../docs/library_index.html#xml_parser)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(xml_parser(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(xml_parser(tester)).


Known issues
------------

When using GNU Prolog as the backend compiler, you may need to to use a
larger default global stack size (see the GNU Prolog documentation on the
environment variable `GLOBALSZ`).
