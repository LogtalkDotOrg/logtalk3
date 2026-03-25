.. _library_application:

``application``
===============

This library provides the ``application_common`` category and the
``application_protocol`` protocol for declaring application metadata,
including optional git-related facts such as repository URL, branch,
commit, author, and commit message, plus optional package and archive
identifiers. Application metadata is typically consumed by tools such as
``sbom``.

The library distinguishes two kinds of information:

- release metadata, such as name, version, description, license,
  distribution location, package identifier, release date, and validity
  date
- optional source provenance metadata, such as repository URL, branch,
  commit, commit date, author, commit message, and archive identifiers

Source provenance predicates are explicit facts declared by the
application. They are not a reflection of the current status of a local
git checkout and should not be assumed to identify a released artifact
unless the application author chooses to state them for that purpose.

API documentation
-----------------

Open the
`../../apis/library_index.html#application <../../apis/library_index.html#application>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(application(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(application(tester)).

Usage
-----

Define an application metadata object importing the
``application_common`` category and declaring the metadata that is known
explicitly. For example:

::

   :- object(my_application,
       imports(application_common)).

       name(my_application).
       version('1.2.3').
       description('Example application metadata object').
       license('Apache-2.0').
       homepage('https://example.com/my_application').
       distribution('https://example.com/my_application/releases/download/v1.2.3/my_application.tgz').
       package('pkg:generic/my_application@1.2.3').
       creators(['Tool: Build pipeline', 'Person: Alice']).
       supplier('Organization: Example Application').
       originator('Organization: Upstream Project').
       repository('https://example.com/my_application.git').
       repository_branch(main).
       repository_commit('0123456789abcdef0123456789abcdef01234567').
       git_object_identifier('gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567').
       software_heritage_identifier('swh:1:rev:0123456789abcdef0123456789abcdef01234567').
       repository_commit_author('Alice').

   :- end_object.

The library distinguishes between creators and originator using two
predicates:

- ``creators/1`` identifies the people, organizations, or tools credited
  with creating the application or preparing its release metadata
- ``originator/1`` identifies the original source of the software when
  that is relevant and distinct from the creators

For example, an internal application might use the same party for both
``creators/1`` and ``originator/1``. A packaged or redistributed
application may use ``creators/1`` for the team or toolchain preparing
the release metadata and ``originator/1`` for the original upstream
source.

The imported category provides a default definition for the
``loader_file/1`` predicate and derived external references that use the
same vocabulary as the corresponding first-class predicates:

- ``external_reference(homepage, URL)`` from ``homepage/1``
- ``external_reference(distribution, URL)`` from ``distribution/1``
- ``external_reference(package, Identifier)`` from ``package/1``
- ``external_reference(repository, URL)`` from ``repository/1``
- ``external_reference(git_object_identifier, Identifier)`` from
  ``git_object_identifier/1``
- ``external_reference(software_heritage_identifier, Identifier)`` from
  ``software_heritage_identifier/1``

The following predicates are intended to help tools such as ``sbom``
export stronger standardized references:

- ``package/1`` Stores an application package identifier as a PURL. This
  is distinct from ``distribution/1``, which stores a download location
  URL. Tools such as ``sbom`` can use ``package/1`` to export a package
  identity reference instead of only a release download location.
  Example: ``package('pkg:generic/my_application@1.2.3')``.
- ``git_object_identifier/1`` Stores a standardized Git object
  identifier as a gitoid. This is distinct from ``repository_commit/1``,
  which stores the raw commit hash as provenance metadata. Tools such as
  ``sbom`` can use ``git_object_identifier/1`` to export a stronger
  provenance reference. Example:
  ``git_object_identifier('gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567')``.
- ``software_heritage_identifier/1`` Stores a Software Heritage
  identifier (SWHID) for an archived release or revision. Tools such as
  ``sbom`` can use it to export a stable archived source provenance
  reference when one is known. Example:
  ``software_heritage_identifier('swh:1:rev:0123456789abcdef0123456789abcdef01234567')``.

Git-related predicates are optional source provenance facts, not
reflection over the current state of a local repository checkout.

Release-oriented metadata typically includes:

- ``name/1``
- ``version/1``
- ``description/1``
- ``license/1``
- ``homepage/1``
- ``distribution/1``
- ``package/1``
- ``creators/1``
- ``supplier/1``
- ``originator/1``
- ``built_date/1``
- ``release_date/1``
- ``valid_until_date/1``
- ``external_reference/2``

Optional source provenance metadata typically includes:

- ``repository/1``
- ``repository_branch/1``
- ``repository_commit/1``
- ``repository_commit_abbreviated/1``
- ``repository_commit_date/1``
- ``repository_commit_author/1``
- ``repository_commit_message/1``
- ``git_object_identifier/1``
- ``software_heritage_identifier/1``
