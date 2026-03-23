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


`sbom`
======

This tool generates a Software Bill of Materials (SBOM) for an application,
exported as a JSON file in the ISO/IEC 5962:2021 standard SPDX 2.3 format:

https://www.iso.org/standard/81870.html


API documentation
-----------------

This tool API documentation is available at:

[../../apis/library_index.html#sbom](../../apis/library_index.html#sbom)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(sbom(loader)).


Testing
-------

To run the tool tests, use the query:

	| ?- logtalk_load(sbom(tester)).


Usage
-----

The tool inspects the current Logtalk session and generates an SPDX document
describing:

- the loaded application
- the Logtalk version
- the backend Prolog system and version
- the installed packs that contributed loaded files to the current session

The public predicates are:

- `document/1`
- `document/2`
- `export/1`
- `export/2`

The `document/1-2` predicates return a JSON term. The `export/1-2` predicates
write the JSON document to any sink accepted by the `json::generate/2`
predicate, including `atom(Atom)` and `file(Path)`.

Supported options are:

- `name(Name)`
  Sets the application package name. Default is `loaded-application`.
- `version(Version)`
  Sets the application package version. Default is `0.0.0`.
- `application_license(License)`
	Sets the application package `licenseConcluded` and `licenseDeclared` SPDX
	identifiers. Default is `NOASSERTION`.
- `logtalk_license(License)`
	Sets the Logtalk package `licenseConcluded` and `licenseDeclared` SPDX
	identifiers. Default is `Apache-2.0`.
- `backend_license(License)`
	Sets the backend Prolog package `licenseConcluded` and `licenseDeclared` SPDX
	identifiers. Default is the license specified in the `backend/3` table.
- `namespace(Namespace)`
  Sets the base document namespace URI. A process and timestamp suffix is added
  automatically to guarantee uniqueness. Default is
  `https://logtalk.org/spdxdocs/logtalk-sbom`.
- `creator(Creator)`
	Sets the `creationInfo.creators` entry. Default is `Logtalk "sbom" tool`.
- `validate_export(Boolean)`
  When `true`, validates the generated document against the bundled SPDX 2.3
  JSON Schema before exporting it. Default is `false`.

Examples:

	| ?- sbom::document(Document).

	| ?- sbom::document(Document, [name(my_app), version('1.2.3')]).

	| ?- sbom::export(file('sbom.json')).

	| ?- sbom::export(atom(Atom), [
	        name(my_app),
	        version('1.2.3'),
	        application_license('MIT'),
	        logtalk_license('Apache-2.0'),
	        backend_license('BSD-2-Clause'),
	        creator('Tool: My build pipeline'),
	        validate_export(true)
	     ]).

See the `sbom-example.json` file for a representative exported document.
