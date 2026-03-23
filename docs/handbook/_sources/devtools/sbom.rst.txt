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

- ``name(Name)`` Sets the application package name. Default is
  ``loaded-application``.
- ``version(Version)`` Sets the application package version. Default is
  ``0.0.0``.
- ``namespace(Namespace)`` Sets the base document namespace URI. A
  process and timestamp suffix is added automatically to guarantee
  uniqueness. Default is ``https://logtalk.org/spdxdocs/logtalk-sbom``.
- ``creator(Creator)`` Sets the ``creationInfo.creators`` entry. Default
  is ``Tool: Logtalk sbom``.
- ``validate_export(Boolean)`` When ``true``, validates the generated
  document against the bundled SPDX 2.3 JSON Schema before exporting it.
  Default is ``false``.

Examples:

::

   | ?- sbom::document(Document).

   | ?- sbom::document(Document, [name(my_app), version('1.2.3')]).

   | ?- sbom::export(file('sbom.json')).

   | ?- sbom::export(atom(Atom), [
           name(my_app),
           version('1.2.3'),
           creator('Tool: My build pipeline'),
           validate_export(true)
        ]).

See the ``sbom-example.json`` file for a representative exported
document.
