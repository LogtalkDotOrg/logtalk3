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
exported as a JSON file in either the ISO/IEC 5962:2021 standard SPDX 2.3
JSON format or the CycloneDX 1.6 JSON format:

https://www.iso.org/standard/81870.html

https://cyclonedx.org/specification/overview/


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

The tool inspects the current Logtalk session and generates a JSON SBOM document
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
predicate, including `atom(Atom)` and `file(Path)`. Follows the list of
supported options.

Global/application options:

- `name(Name)`
    Sets the application name. Exported as the SPDX application package name
    and as the CycloneDX `metadata.component.name`. Default is
    `loaded-application`.
- `format(Format)`
	Selects the export format. Possible values are `spdx` and `cyclonedx`.
	Default is `spdx`.
- `version(Version)`
    Sets the application version. Exported as the SPDX application package
    `versionInfo` and as the CycloneDX `metadata.component.version`. Default is
    `0.0.0`.
- `application_license(License)`
	Sets the application license. Exported as the SPDX application package
	`licenseConcluded` and `licenseDeclared` fields and, unless the value is
	`NOASSERTION`, as the CycloneDX `metadata.component.licenses` entry. Default
	is `NOASSERTION`.
- `application_built_date(Date)`
	Sets the application build date. Exported as the SPDX application package
	`builtDate` field and as the CycloneDX custom property
	`logtalk:sbom:built_date`. Default is not exporting this information.
- `application_release_date(Date)`
	Sets the application release date. Exported as the SPDX application package
	`releaseDate` field and as the CycloneDX custom property
	`logtalk:sbom:release_date`. Default is not exporting this information.
- `application_valid_until_date(Date)`
	Sets the application validity limit date. Exported as the SPDX application
	package `validUntilDate` field and as the CycloneDX custom property
	`logtalk:sbom:valid_until_date`. Default is not exporting this information.
- `application_supplier(Supplier)`
	Sets the application supplier. Exported as the SPDX application package
	`supplier` field. For CycloneDX, exported as `metadata.component.supplier`
	when using the `Organization: Name` convention and also as the custom
	property `logtalk:sbom:supplier`. Default is not exporting this
	information.
- `application_originator(Originator)`
	Sets the application originator. Exported as the SPDX application package
	`originator` field. For CycloneDX, exported as
	`metadata.component.manufacturer` when using the `Organization: Name`
	convention, or as `metadata.component.authors` when using the
	`Person: Name` convention, and also as the custom property
	`logtalk:sbom:originator`. Default is not exporting this information.
- `namespace(Namespace)`
    Sets the base document namespace URI. A process and timestamp suffix is added
    automatically to guarantee uniqueness. This option only applies to SPDX
	exports (`documentNamespace`) and is ignored for CycloneDX exports. Default is
    `https://logtalk.org/spdxdocs/logtalk-sbom`.
- `creator(Creator)`
	Sets the SPDX `creationInfo.creators` entry and the CycloneDX
	`metadata.authors` entry. Default is `Logtalk "sbom" tool`.
- `validate_export(Boolean)`
	When `true`, validates the generated document against the bundled schema for
	the selected format before exporting it. Default is `false`.

Logtalk options:

- `logtalk_license(License)`
	Sets the Logtalk component license. Exported as the SPDX Logtalk package
	`licenseConcluded` and `licenseDeclared` fields and, unless the value is
	`NOASSERTION`, as the CycloneDX component `licenses` entry. Default is
	`Apache-2.0`.
- `logtalk_built_date(Date)`
	Sets the Logtalk build date. Exported as the SPDX Logtalk package
	`builtDate` field and as the CycloneDX custom property
	`logtalk:sbom:built_date`. Default is not exporting this information.
- `logtalk_release_date(Date)`
	Sets the Logtalk release date. Exported as the SPDX Logtalk package
	`releaseDate` field and as the CycloneDX custom property
	`logtalk:sbom:release_date`. Default is not exporting this information.
- `logtalk_valid_until_date(Date)`
	Sets the Logtalk validity limit date. Exported as the SPDX Logtalk package
	`validUntilDate` field and as the CycloneDX custom property
	`logtalk:sbom:valid_until_date`. Default is not exporting this information.
- `logtalk_supplier(Supplier)`
	Sets the Logtalk supplier. Exported as the SPDX Logtalk package `supplier`
	field. For CycloneDX, exported as the component `supplier` when using the
	`Organization: Name` convention and also as the custom property
	`logtalk:sbom:supplier`. Default is not exporting this information.
- `logtalk_originator(Originator)`
	Sets the Logtalk originator. Exported as the SPDX Logtalk package
	`originator` field. For CycloneDX, exported as the component `manufacturer`
	or `authors` entry, depending on the value convention, and also as the custom
	property `logtalk:sbom:originator`. Default is not exporting this
	information.

Backend options:

- `backend_license(License)`
	Sets the backend component license. Exported as the SPDX backend package
	`licenseConcluded` and `licenseDeclared` fields and, unless the value is
	`NOASSERTION`, as the CycloneDX component `licenses` entry. Default is the
	license specified in the `backend/3` table.
- `backend_built_date(Date)`
	Sets the backend build date. Exported as the SPDX backend package
	`builtDate` field and as the CycloneDX custom property
	`logtalk:sbom:built_date`. Default is not exporting this information.
- `backend_release_date(Date)`
	Sets the backend release date. Exported as the SPDX backend package
	`releaseDate` field and as the CycloneDX custom property
	`logtalk:sbom:release_date`. Default is not exporting this information.
- `backend_valid_until_date(Date)`
	Sets the backend validity limit date. Exported as the SPDX backend package
	`validUntilDate` field and as the CycloneDX custom property
	`logtalk:sbom:valid_until_date`. Default is not exporting this information.
- `backend_supplier(Supplier)`
	Sets the backend supplier. Exported as the SPDX backend package `supplier`
	field. For CycloneDX, exported as the component `supplier` when using the
	`Organization: Name` convention and also as the custom property
	`logtalk:sbom:supplier`. Default is not exporting this information.
- `backend_originator(Originator)`
	Sets the backend originator. Exported as the SPDX backend package
	`originator` field. For CycloneDX, exported as the component `manufacturer`
	or `authors` entry, depending on the value convention, and also as the custom
	property `logtalk:sbom:originator`. Default is not exporting this
	information.

Pack options:

- `pack_license(Pack, License)`
	Sets the license for a loaded pack named `Pack`. Exported as the SPDX pack
	package `licenseConcluded` and `licenseDeclared` fields and, unless the value
	is `NOASSERTION`, as the CycloneDX component `licenses` entry. Default for
	packs without an explicit option is the result of sending the pack
	specification object the message `license(License)`, falling back to
	`NOASSERTION` when no license is available. Loaded packs also export a SPDX
	package checksum and a CycloneDX component hash when the pack specification
	defines it in the `version/6` predicate as the fourth argument.
- `pack_built_date(Pack, Date)`
	Sets the build date for the loaded pack named `Pack`. Exported as the SPDX
	pack package `builtDate` field and as the CycloneDX custom property
	`logtalk:sbom:built_date`. Default is not exporting this information.
- `pack_release_date(Pack, Date)`
	Sets the release date for the loaded pack named `Pack`. Exported as the SPDX
	pack package `releaseDate` field and as the CycloneDX custom property
	`logtalk:sbom:release_date`. Default is not exporting this information.
- `pack_valid_until_date(Pack, Date)`
	Sets the validity limit date for the loaded pack named `Pack`. Exported as
	the SPDX pack package `validUntilDate` field and as the CycloneDX custom
	property `logtalk:sbom:valid_until_date`. Default is not exporting this
	information.
- `pack_supplier(Pack, Supplier)`
	Sets the supplier for the loaded pack named `Pack`. Exported as the SPDX pack
	package `supplier` field. For CycloneDX, exported as the component
	`supplier` when using the `Organization: Name` convention and also as the
	custom property `logtalk:sbom:supplier`. Default is not exporting this
	information.
- `pack_originator(Pack, Originator)`
	Sets the originator for the loaded pack named `Pack`. Exported as the SPDX
	pack package `originator` field. For CycloneDX, exported as the component
	`manufacturer` or `authors` entry, depending on the value convention, and
	also as the custom property `logtalk:sbom:originator`. Default is not
	exporting this information.

Examples:

	| ?- sbom::document(Document).

	| ?- sbom::document(Document, [name(my_app), version('1.2.3')]).

	| ?- sbom::export(file('sbom.spdx.json')).

	| ?- sbom::export(file('sbom.cdx.json'), [format(cyclonedx)]).

	| ?- sbom::export(atom(Atom), [
	        format(cyclonedx),
	        name(my_app),
	        version('1.2.3'),
	        application_license('MIT'),
	        logtalk_license('Apache-2.0'),
	        backend_license('BSD-2-Clause'),
	        application_built_date('2026-03-23T00:00:00Z'),
	        application_release_date('2026-03-23T00:00:00Z'),
	        application_valid_until_date('2027-03-23T00:00:00Z'),
	        application_supplier('Organization: Example Application'),
	        application_originator('Person: Application Maintainer'),
	        logtalk_supplier('Organization: Logtalk.org'),
	        backend_supplier('Organization: Backend Vendor'),
	        pack_license(my_pack, 'MIT'),
	        pack_supplier(my_pack, 'Organization: Pack Maintainer'),
	        creator('Tool: My build pipeline'),
	        validate_export(true)
	     ]).

Use the `.spdx.json` extension for SPDX exports and the `.cdx.json` extension
for CycloneDX exports.

See the `sbom-example.spdx.json` file for a representative SPDX export.
See the `sbom-example.cdx.json` file for a representative CycloneDX export.

