.. _library_sbom:

``sbom``
========

This tool generates a Software Bill of Materials (SBOM) for an
application, exported as a JSON file in the ISO/IEC 5962:2021 standard
SPDX 2.3 format:

https://www.iso.org/standard/81870.html

API documentation
-----------------

This tool API documentation is available at:

`../../apis/library_index.html#sbom <../../apis/library_index.html#sbom>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(sbom(loader)).

Testing
-------

To run the tool tests, use the query:

::

   | ?- logtalk_load(sbom(tester)).

Usage
-----

The tool inspects the current Logtalk session and generates an SPDX
document describing:

- the loaded application
- the Logtalk version
- the backend Prolog system and version
- the installed packs that contributed loaded files to the current
  session

The public predicates are:

- ``document/1``
- ``document/2``
- ``export/1``
- ``export/2``

The ``document/1-2`` predicates return a JSON term. The ``export/1-2``
predicates write the JSON document to any sink accepted by the
``json::generate/2`` predicate, including ``atom(Atom)`` and
``file(Path)``.

Supported options are:

Global/application options:

- ``name(Name)`` Sets the application package name. Default is
  ``loaded-application``.
- ``version(Version)`` Sets the application package version. Default is
  ``0.0.0``.
- ``application_license(License)`` Sets the application package
  ``licenseConcluded`` and ``licenseDeclared`` SPDX identifiers. Default
  is ``NOASSERTION``.
- ``application_built_date(Date)`` Sets the application package
  ``builtDate`` field. Default is not exporting this field.
- ``application_release_date(Date)`` Sets the application package
  ``releaseDate`` field. Default is not exporting this field.
- ``application_valid_until_date(Date)`` Sets the application package
  ``validUntilDate`` field. Default is not exporting this field.
- ``application_supplier(Supplier)`` Sets the application package
  ``supplier`` field. Default is not exporting this field.
- ``application_originator(Originator)`` Sets the application package
  ``originator`` field. Default is not exporting this field.
- ``namespace(Namespace)`` Sets the base document namespace URI. A
  process and timestamp suffix is added automatically to guarantee
  uniqueness. Default is ``https://logtalk.org/spdxdocs/logtalk-sbom``.
- ``creator(Creator)`` Sets the ``creationInfo.creators`` entry. Default
  is ``Logtalk "sbom" tool``.
- ``validate_export(Boolean)`` When ``true``, validates the generated
  document against the bundled SPDX 2.3 JSON Schema before exporting it.
  Default is ``false``.

Logtalk options:

- ``logtalk_license(License)`` Sets the Logtalk package
  ``licenseConcluded`` and ``licenseDeclared`` SPDX identifiers. Default
  is ``Apache-2.0``.
- ``logtalk_built_date(Date)`` Sets the Logtalk package ``builtDate``
  field. Default is not exporting this field.
- ``logtalk_release_date(Date)`` Sets the Logtalk package
  ``releaseDate`` field. Default is not exporting this field.
- ``logtalk_valid_until_date(Date)`` Sets the Logtalk package
  ``validUntilDate`` field. Default is not exporting this field.
- ``logtalk_supplier(Supplier)`` Sets the Logtalk package ``supplier``
  field. Default is not exporting this field.
- ``logtalk_originator(Originator)`` Sets the Logtalk package
  ``originator`` field. Default is not exporting this field.

Backend options:

- ``backend_license(License)`` Sets the backend Prolog package
  ``licenseConcluded`` and ``licenseDeclared`` SPDX identifiers. Default
  is the license specified in the ``backend/3`` table.
- ``backend_built_date(Date)`` Sets the backend package ``builtDate``
  field. Default is not exporting this field.
- ``backend_release_date(Date)`` Sets the backend package
  ``releaseDate`` field. Default is not exporting this field.
- ``backend_valid_until_date(Date)`` Sets the backend package
  ``validUntilDate`` field. Default is not exporting this field.
- ``backend_supplier(Supplier)`` Sets the backend package ``supplier``
  field. Default is not exporting this field.
- ``backend_originator(Originator)`` Sets the backend package
  ``originator`` field. Default is not exporting this field.

Pack options:

- ``pack_license(Pack, License)`` Sets the ``licenseConcluded`` and
  ``licenseDeclared`` SPDX identifiers for a loaded pack named ``Pack``.
  Default for packs without an explicit option is the result of sending
  the pack specification object the message ``license(License)``,
  falling back to ``NOASSERTION`` when no license is available. Loaded
  packs also export a SPDX package checksum when the pack specification
  defines it in the ``version/6`` predicate as the fourth argument.
- ``pack_built_date(Pack, Date)`` Sets the ``builtDate`` field for the
  loaded pack named ``Pack``. Default is not exporting this field.
- ``pack_release_date(Pack, Date)`` Sets the ``releaseDate`` field for
  the loaded pack named ``Pack``. Default is not exporting this field.
- ``pack_valid_until_date(Pack, Date)`` Sets the ``validUntilDate``
  field for the loaded pack named ``Pack``. Default is not exporting
  this field.
- ``pack_supplier(Pack, Supplier)`` Sets the ``supplier`` field for the
  loaded pack named ``Pack``. Default is not exporting this field.
- ``pack_originator(Pack, Originator)`` Sets the ``originator`` field
  for the loaded pack named ``Pack``. Default is not exporting this
  field.

Examples:

::

   | ?- sbom::document(Document).

   | ?- sbom::document(Document, [name(my_app), version('1.2.3')]).

   | ?- sbom::export(file('sbom.spdx.json')).

   | ?- sbom::export(atom(Atom), [
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

See the ``sbom-example.spdx.json`` file for a representative exported
document.
