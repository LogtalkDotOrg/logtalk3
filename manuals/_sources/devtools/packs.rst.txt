``packs``
=========

This tool provides predicates for downloading, installing, upgrading,
and removing third-party libraries and applications.

Requirements
------------

On POSIX systems (Linux, macOS, ...), the following command-line
commands are required:

-  ``sha256sum`` (provided by GNU coreutils)
-  ``curl``
-  ``bsdtar`` (unlike GNU ``tar``, it can uncompress ``.zip`` archives)

On Windows systems, the following command-line commands are required:

-  ``certutil.exe``
-  ``curl.exe``
-  ``tar.exe``

No install should be required in recent Windows 10 builds.

On macOS systems, Apple bundles both ``curl`` and BSD ``tar`` (under the
name ``tar``). GNU coreutils can be installed easily using MacPorts
(``sudo port install coretutils``) or Homebrew
(``brew install coretutils``).

On Linux systems, use the distribution own package manager to install
any missing command.

Additionally, if you want to check pack archive signatures, the ``gpg``
command-line command is also required. The GnuPG software can be
download from:

https://www.gnupg.org/

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#packs <../../docs/library_index.html#packs>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(packs(loader)).

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(packs(tester)).

Usage
-----

The tool provides two main objects, ``registries`` and ``packs``, for
handling, respectively, registries and packs. Both objects accept a
``help/0`` message. For example:

Registries and packs storage
----------------------------

The tool uses a directory specified using the ``LOGTALKPACKS``
environment variable when defined. Otherwise it defaults to
``~/logtalk_packs``. This directory holds sub-directories for
registries, packs, and archives. Users shouldn't manually modify the
contents of these directories.

Registry specification
----------------------

A registry is git remote repo that can be cloned, a downloadable
archive, or a local directory containing a Logtalk loader file that
loads source files defining the registry itself and the packs it
provides. The registry name is ideally a valid unquoted atom. The
registry directory must contain at least two Logtalk source files:

-  A file defining an object named after the registry with a
   ``_registry`` suffix, implementing the ``registry_protocol``.

-  A loader file (named ``loader.lgt`` or ``loader.logtalk``) that loads
   the registry object file and all pack object files.

The registry directory should also contain ``LICENSE`` and ``README.md``
files (individual packs can use a different license, however). The path
to the ``README.md`` file is printed when the registry is added.

Pack specification
------------------

A pack is specified using a Logtalk source file defining an object that
implements the ``pack_protocol``. The source file should be named after
the pack with a ``_pack`` suffix. The file must be available from a
declared pack registry. The pack name is ideally a valid unquoted atom.

The pack sources must be available for downloading as a ``.zip``,
``.tar.gz``, or ``.tar.bz2`` archive. The checksum for the archive must
use the SHA-256 hash algorithm (``sha256``). The pack may optionally be
signed.

When the pack sources contains a ``README.md`` file, the path to this
file is printed when the pack is installed or updated.

Pack versions
-------------

A pack may specify multiple versions. Each version is described using a
``version/6`` predicate clause. For details, see the ``pack_protocol``
API documentation.

Pack dependencies
-----------------

Pack dependencies can be specified by pack dependency name or, for
better security, by registry and dependency names. When the pack depends
on the Logtalk version itself, the reserved name ``logtalk`` can be
used.

-  ``Registry::Pack >= Version`` - the pack requires a dependency with
   version equal or above the specified one. For example,
   ``logtalk >= 3:36:0`` means that the pack requires Logtalk 3.36.0 or
   later version.

-  ``Registry::Pack =< Version`` - the pack requires a dependency with
   version up to the specified one. For example, ``common::bits =< 2:1``
   means that the pack requires a ``common::bits`` pack up to 2.1. This
   includes all previous versions and also all patches for version 2.1
   (e.g. 2.1.7, 2.1.8, ...) but not version 2.2 or newer.

-  ``Registry::Pack < Version`` - the pack requires a dependency with
   version older than the specified one. For example,
   ``common::bits < 3`` means that the pack requires a ``common::bits``
   2.x or older version.

-  ``Registry::Pack > Version`` - the pack requires a dependency with
   version newer than the specified one. For example,
   ``common::bits > 2.4`` means that the pack requires a
   ``common::bits`` 2.5 or newer version.

-  ``Registry::Pack = Version`` - the pack requires a dependency with a
   specific version. For example, ``common::bits = 2.1`` means that the
   pack requires a ``common::bits`` pack version 2.1.x (thus, from
   version 2.1.0 to the latest patch for version 2.1).

Dependencies are specified using a list of the elements above. For
example, ``[common::bits >= 2, common::bits < 3]`` means all 2.x
versions but not older or newer versions.

Registry handling
-----------------

Registries can be added using the ``registries::add/2`` predicate, which
takes a registry name and a registry URL. For example:

::

   | ?- registries::add(reg, 'https://github.com/some_user/reg.git').

Git cloning URLs are preferred but a registry can also be made available
via a local archive (using a ``file://`` URL) or a downloadable archive
(using a ``https://`` URL).

To update a registry, use the ``registries::update/1-2`` predicates.
Registries can also be deleted using the ``registries::delete/1-2``
predicate. After deleting a registry, you can use the
``packs::orphaned/0`` predicate to list any orphaned packs that are
installed.

Pack handling
-------------

Packs must of available from a defined registry. To list all packs that
are available for installation, use the ``packs::available/0``
predicate:

::

   | ?- packs::available.

To know more about a specific pack, use the ``packs::describe/1-2``
predicates. For example:

::

   | ?- packs::describe(bar).

The ``packs::describe/2`` predicate can be used when two or more
registries provide packs with the same name. For example:

::

   | ?- packs::describe(reg, bar).

To install the latest version of a pack, assuming unique among all
defined registries, we can use the ``packs::install/1-4`` predicates.
For example:

::

   | ?- packs::install(bar).

Other install predicates are available to disambiguate between
registries and to install a specific pack version. The
``packs::installed/0`` predicate can be used to list all installed
packs.

To update a pack, use the ``packs::update/1-2`` predicates. For example:

::

   | ?- packs::update(bar).

The tool provides versions of the pack install, update, and uninstall
predicates that accept a list of options:

-  ``verbose`` (default is ``false``)
-  ``clean`` (default is ``false``)
-  ``force`` (default is ``false``)
-  ``checksum`` (default is ``true``)
-  ``checksig`` (default is ``false``)

When using a ``checksig(true)`` option to check a pack signature, is
strongly advised that you also use the ``verbose(true)`` option. Note
that the public key used to sign the pack archive must be already
present in your local system.

Security considerations
-----------------------

New pack registries should be examined before being added, specially if
public and from a previously unknown source. Same when adding or
updating a pack. But note that a registry can always index third-party
packs.

Pack checksums are checked by default. But pack signatures are only
checked if requested as packs are often unsigned. Care should be taken
when adding public keys for pack signers to your local system.

Registry and pack spec files plus the registry loader file are compiled
by term-expanding them so that only expected terms are actually loaded
and only expected ``logtalk_load/2`` goals are allowed. Predicates
defining URLs are discarded if the URLs are neither ``https://`` nor
``file://`` URLs or if they contain URL search parameters. But note that
this tool makes no attempt to audit pack source files themselves.

Registries and packs can always be pinned so that they are not
accidentally updated to a version that you may not had the chance to
audit.

Best practices
--------------

-  Make available a new pack registry as a git repo. This simplifies
   updating the registry and rolling back to a previous version.

-  Create new pack versions from git tags.

-  If the sources of a pack are available from a git repo, consider
   using signed commits and signed tags for increased security.

-  When a new pack version breaks backwards compatibility, list both the
   old and the new versions on the pack spec file.

-  Pin registries and packs when specific versions are critical for your
   work so that you can still easily batch update the remaining packs
   and registries.

-  Include the ``$LOGTALKPACKS`` directory (or the default
   ``~/logtalk_packs`` directory) on your regular backups.
