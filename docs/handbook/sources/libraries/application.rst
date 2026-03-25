.. _library_application:

``application``
===============

This library provides the ``application_common`` category and the
``application_protocol`` protocol for declaring application metadata,
including optional git-related facts such as repository URL, branch,
commit, author, and commit message. Application metadata is typically
consumed by tools such as ``sbom``.

The library distinguishes two kinds of information:

- release metadata, such as name, version, description, license,
  distribution location, release date, and validity date
- optional source provenance metadata, such as repository URL, branch,
  commit, commit date, author, and commit message

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
       creators(['Tool: Build pipeline', 'Person: Alice']).
       supplier('Organization: Example Application').
       originator('Organization: Upstream Project').
       repository('https://example.com/my_application.git').
       repository_branch(main).
       repository_commit('0123456789abcdef0123456789abcdef01234567').
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
- ``external_reference(repository, URL)`` from ``repository/1``

Git-related predicates are optional source provenance facts, not
reflection over the current state of a local repository checkout.

Release-oriented metadata typically includes:

- ``name/1``
- ``version/1``
- ``description/1``
- ``license/1``
- ``homepage/1``
- ``distribution/1``
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
