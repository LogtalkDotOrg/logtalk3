.. _packs:

``packs``
=========

This tool provides predicates for downloading, installing, upgrading,
and uninstalling third-party libraries and applications, generically
known as *packs*. Collections of pack specifications are made available
using *registries*. Registries can be local to a system, publicly
shared, or private to a company VPN. There is no concept of a central
registry. Users decide which registries they trust and want to use and
add them using their published URLs. The tool supports both pack
checksums and signatures and takes several steps to sanitize registry
and pack specifications. As other Logtalk developer tools, portability
is a main goal. This tool can be used with any supported Prolog backend
and run in both POSIX and Windows systems. Moreover, this tool can be
used not only for handling Logtalk packs but also Prolog only packs,
thus providing a solution for sharing portable resources between
multiple systems.

A list of public Logtalk and Prolog pack registries is available at:

https://github.com/LogtalkDotOrg/pack-registries

This tool is the beta stage of development.

Requirements
------------

On POSIX systems (Linux, macOS, ...), the following shell commands are
required:

-  ``sha256sum`` (provided by GNU ``coreutils``)
-  ``curl``
-  ``bsdtar`` (provided by ``libarchive`` or ``libarchive-tools``)
-  ``gpg`` (provided by ``gnupg2``)
-  ``git``

The tool uses ``bsdtar`` instead of GNU ``tar`` so that it can
uncompress ``.zip`` archives (``unzip`` doesn't provide the desired
options that allows a simple and reliable solution for ignoring the
non-predictable name of the wrapper directory).

On Windows systems, the following shell commands are required:

-  ``certutil.exe``
-  ``curl.exe``
-  ``tar.exe``
-  ``gpg``
-  ``git``

In recent Windows 10 builds, only ``gpg`` and ``git`` should require
installation. You can download the GnuPG software from:

https://www.gnupg.org/

You can download Git from:

https://gitforwindows.org

On macOS systems, Apple bundles both ``curl`` and BSD ``tar`` (under the
name ``tar``; you can simply create a ``bsdtar`` alias or install a more
recent version). The required commands can be easily installed using
MacPorts:

::

   $ sudo port install coreutils libarchive gnupg2

Or using Homebrew:

::

   $ brew install coretutils libarchive gnupg2

On Linux systems, use the distribution own package manager to install
any missing command.

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

To run the tool tests, use the query:

::

   | ?- logtalk_load(packs(tester)).

The tests can be run without interfering with the user packs setup.

Usage
-----

The ``packs`` tool loads at startup all the currently defined registry
and pack specifications (from the registries/packs storage directory;
see below). When no registry/pack setup exists, a new one is
automatically created.

The tool provides two main objects, ``registries`` and ``packs``, for
handling, respectively, registries and packs. Both objects accept a
``help/0`` message that describes the most common queries.

Registries and packs storage
----------------------------

The tool uses a directory specified using the ``logtalk_packs`` library
alias when defined (in a settings file or in a backend Prolog
initialization file). When this library alias is not defined, the tool
uses the value of the ``LOGTALKPACKS`` environment variable when
defined. Otherwise it defaults to the ``~/logtalk_packs`` directory. The
actual directory can be retrieved by the query:

::

   | ?- packs::logtalk_packs(Directory).
   ...

This directory holds sub-directories for registries, packs, and
archives. These sub-directories are automatically created when loading
the ``packs`` tool if they don't exist . Users shouldn't manually modify
the contents of these directories. Multiple and independent
registry/pack setups are possible using *virtual environments* as
explained next.

Your registries and packs setup can be saved and restored (e.g. in a
different system) by using the ``packs::save/2`` and
``packs::restore/1-2`` predicates. For example:

::

   | ?- packs::save(my_setup, [save(all)]).
   ...

Then, in the destination system:

::

   | ?- packs::restore(my_setup).
   ...

If necessary, before restoring, the ``packs::reset/0`` predicate can be
called to delete any defined registries and installed packs.

Virtual environments
--------------------

An application may require a specific Logtalk version (e.g. the version
used to test and certify it) and specific pack versions. These
requirements may differ between applications. Different applications may
also have conflicting requirements. Therefore, a *virtual environment*
where an application requirements are fulfilled may be required to
develop and/or run it. A virtual environment is essentially a
registries/packs storage directory.

Defining the ``logtalk_packs`` library alias in a settings file or
defining the ``LOGTALKPACKS`` environment variable before starting
Logtalk allows easy creation and switching between virtual environments.
By using a per application settings file (or a per application
environment variable definition) each application can thus use its own
virtual environment.

When a virtual environment also requires a specific Logtalk version,
this can be installed as a pack from the official
`talkshow <https://github.com/LogtalkDotOrg/talkshow>`__ registry and
used by (re)defining the ``LOGTALKHOME`` and ``LOGTALKUSER`` environment
variables to point to its pack directory (which can be queried by using
the ``packs::directory/2`` message). Several shell utilities are
available that can set environment variables when changing to an
application directory (see e.g.
`direnv <https://github.com/direnv/direnv>`__).

A virtual environment setup (i.e. the currently defined registries and
installed packs) can be saved into a file (e.g. ``requirements.lgt``)
using the ``packs::save/1`` predicate:

::

   | ?- packs::save('requirements.lgt').
   ...

This query saves a listing of all the installed packs and their
registries. Using the saved file, the virtual environment setup can then
be restored using the ``packs::restore/1-2`` predicates. The file uses a
simple format with ``registry/2`` and ``pack/3`` facts (in this order)
and can be manually created or edited if necessary. For example:

::

   registry(talkshow, 'https://github.com/LogtalkDotOrg/talkshow.git').
   pack(talkshow, logtalk, 3:45:0).
   pack(talkshow, lflat, 2:1:0).

These files can be distributed with applications so that users can
easily fulfill application requirements by using the ``packs`` tool.
Typically, an application directory will include ``settings.lgt`` and
``requirements.lgt`` files. The ``settings.lgt`` file can define the
``logtalk_packs`` library alias using code such as:

::

   :- initialization((
       logtalk_load_context(directory, Directory),
       assertz(logtalk_library_path(logtalk_packs, Directory))
   )).

A suitable named sub-directory can also be used. The application
requirements can then be fulfilled by starting Logtalk from the
application directory (so that the application settings file is loaded)
and running once the query:

::

   | ?- packs::restore('requirements.lgt').

After, the application ``loader.lgt`` file can then load the required
packs using their loader files:

::

   :- initialization((
       % load required packs
       logtalk_load(foo(loader)),
       logtalk_load(bar(loader)),
       ...
       % load application files
       ...
   )).

Registry specification
----------------------

A registry is a git remote repo that can be cloned, a downloadable
archive, or a local directory containing a Logtalk loader file that
loads source files defining the registry itself and the packs it
provides. The registry name is ideally a valid unquoted atom. The
registry directory must contain at least two Logtalk source files:

-  A file defining an object named after the registry with a
   ``_registry`` suffix, implementing the ``registry_protocol``. This
   naming convention helps preventing name conflicts.

-  A loader file (named ``loader.lgt`` or ``loader.logtalk``) that loads
   the registry object file and all pack object files.

An example of a registry specification object would be:

::

   :- object(jdoe_awesome_packs_registry,
       implements(registry_protocol)).

       :- info([
           version is 1:0:0,
           author is 'John Doe',
           date is 2021-10-18,
           comment is 'John Doe awesome packs registry spec.'
       ]).

       name(jdoe_awesome_packs).

       description('John Doe awesome packs').

       home('https://example.com/jdoe_awesome_packs').

       clone('https://github.com/jdoe/jdoe_awesome_packs.git').

       archive('https://github.com/jdoe/jdoe_awesome_packs/archive/main.zip').

   :- end_object.

Optionally, the registry object can also define a ``note(Action, Note)``
predicate. The ``Action`` argument is an atom: ``add``, ``update``, or
``delete``. The ``Note`` argument is also an atom. The tool will print
any available notes when executing one of the registry actions. See the
``registry_protocol`` documentation for more details.

The registry directory should also contain ``LICENSE`` and ``README.md``
files (individual packs can use a different license, however). The path
to the ``README.md`` file is printed when the registry is added. It can
also be queried using the ``registries::directory/2`` predicate.

Summarizing the required directory structure using the above example
(note that the registry and pack specification files are named after the
objects):

::

   jdoe_awesome_packs
       LICENSE
       README.md
       jdoe_awesome_packs_registry.lgt
       loader.lgt
       foo_pack.lgt
       bar_pack.lgt
       ...

With the contents of the ``loader.lgt`` file being:

::

   :- initialization((
       logtalk_load(jdoe_awesome_packs_registry),
       logtalk_load(foo_pack),
       logtalk_load(bar_pack),
       ...
   )).

It would be of course possible to have all objects in a single source
file. But having a file per object and a loader file helps maintenance
and it's also a tool requirement for applying safety procedures to the
source file contents and thus successfully loding the registry and pack
specs.

As registries are git repos in the most common case and thus adding them
performs a git repo cloning, they should only contain the strictly
required files.

Registry handling
-----------------

Registries can be added using the ``registries::add/1-3`` predicates,
which take a registry URL. Using the example above:

::

   | ?- registries::add('https://github.com/jdoe/jdoe_awesome_packs.git').

HTTPS URLs must end with either a ``.git`` extension or a an archive
extension. Git cloning URLs are preferred but a registry can also be
made available via a local directory (using a ``file://`` URL) or a
downloadable archive (using a ``https://`` URL).

For registries made available using an archive, the
``registries::add/2-3`` predicates **must** be used as the registry name
cannot in general be inferred from the URL basename or from the archived
directory name. The registry argument must also be the declared registry
name in the registry specification object. For example:

::

   | ?- registries::add(
           jdoe_awesome_packs,
           'https://github.com/jdoe/jdoe_awesome_packs/archive/main.zip'
        ).

The added registries can be listed using the ``registries::list/0``
predicate:

::

   | ?- registries::list.

   % Defined registries:
   %   jdoe_awesome_packs (git)
   %   ...

The ``registries::describe/1`` predicate can be used to print the
details of a registry:

::

   | ?- registries::describe(jdoe_awesome_packs).

   % Registry:    jdoe_awesome_packs
   % Description: John Doe awesome packs
   % Home:        https://example.com/jdoe_awesome_packs
   % Cloning URL: https://github.com/jdoe/jdoe_awesome_packs.git
   % Archive URL: https://github.com/jdoe/jdoe_awesome_packs/archive/main.zip

To update all registries, use the ``registries::update/0`` predicate. To
update a single registry, use the ``registries::update/1-2`` predicates.
After updating, you can use the ``packs::outdated/0-1`` predicates to
list any outdated packs.

Registries can also be deleted using the ``registries::delete/1-2``
predicate. By default, any registries with installed packs cannot be
deleted. If you force deletion (by using the ``force(true)`` option),
you can use the ``packs::orphaned/0`` predicate to list any orphaned
packs that are installed.

See the tool API documentation on the
`registries <../../docs/registries_0.html>`__ object for other useful
predicates.

Registry development
--------------------

To simplify registry development and testing, use a local directory and
a ``file://`` URL when calling the ``registries::add/1`` predicate. For
example:

::

   | ?- registries::add('file:///home/jdoe/work/my_pack_collection').

If the directory is a git repo, the tool will clone it when adding it.
Otherwise, the files in the directory are copied to the registry
definition directory. This allows the registry to be added and deleted
without consequences for the original registry source files.

To check your registry specifications, use the ``registries::lint/0-1``
predicates after adding the registry.

Pack specification
------------------

A pack is specified using a Logtalk source file defining an object that
implements the ``pack_protocol``. The source file should be named after
the pack with a ``_pack`` suffix. This naming convention helps
preventing name conflicts, notably with the pack own objects. The file
must be available from a declared pack registry. The pack name is
ideally a valid unquoted atom. An example of a registry specification
object would be:

::

   :- object(lflat_pack,
       implements(pack_protocol)).

       :- info([
           version is 1:0:0,
           author is 'Paulo Moura',
           date is 2021-10-18,
           comment is 'L-FLAT - Logtalk Formal Language and Automata Toolkit pack spec.'
       ]).

       name(lflat).

       description('L-FLAT - Logtalk Formal Language and Automata Toolkit').

       license('MIT').

       home('https://github.com/l-flat/lflat').

       version(
           2:1:0,
           stable,
           'https://github.com/l-flat/lflat/archive/refs/tags/v2.1.0.tar.gz',
           sha256 - '9c298c2a08c4e2a1972c14720ef1498e7f116c7cd8bf7702c8d22d8ff549b6a1',
           [logtalk @>= 3:36:0],
           all
       ).

       version(
           2:0:2,
           stable,
           'https://github.com/l-flat/lflat/archive/refs/tags/v2.0.2.tar.gz',
           sha256 - '8774b3863efc03bb6c284935885dcf34f69f115656d2496a33a446b6199f3e19',
           [logtalk @>= 3:36:0],
           all
       ).

   :- end_object.

Optionally, the pack object can also define a
``note(Action, Version, Note)`` predicate. The ``Action`` argument is an
atom: ``install``, ``update``, or ``uninstall``. The ``Note`` argument
is also an atom. The tool will print any available notes when executing
one of the registry actions. See the ``pack_protocol`` documentation for
more details.

The pack sources must be available either as a local directory (when
using a ``file://`` URL) or for downloading as a supported archive. The
checksum for the archive must use the SHA-256 hash algorithm
(``sha256``). The pack may optionally be signed. Supported archive
formats and extensions are:

-  ``.zip``
-  ``.tgz``, ``.tar.gz``
-  ``.tbz2``, ``.tar.bz2``

The pack sources should contain ``LICENSE``, ``README.md``, and
``loader.lgt`` (or ``loader.logtalk``) files. The path to the
``README.md`` file is printed when the pack is installed or updated. It
can also be queried using the ``packs::directory/2`` predicate.

Pack versions
-------------

A pack may specify multiple versions. Each version is described using a
``version/6`` predicate clause as illustrated in the example above. The
versions must be listed ordered from newest to oldest. For details, see
the ``pack_protocol`` API documentation.

Listing multiple versions allows the pack specification to be updated
(by updating its registry) without forcing existing users into
installing (or updating to) the latest version of the pack.

Pack dependencies
-----------------

Pack dependencies on other packs can be specified using the syntax
``Registry::Pack Operator Version`` where ``Operator`` is a standard
term comparison operator as described below. When a pack depends on a
Logtalk or backend version, the name ``logtalk`` or the identifier of
the backend can be used in place of ``Registry::Pack`` (see below for
the table of backend specifiers).

Dependencies are specified using a list of the following elements:

-  ``Registry::Pack @>= Version`` - the pack requires a dependency with
   version equal or above the specified one. For example,
   ``logtalk @>= 3:36:0`` means that the pack requires Logtalk 3.36.0 or
   later version.

-  ``Registry::Pack @=< Version`` - the pack requires a dependency with
   version up to the specified one. For example,
   ``common::bits @=< 2:1`` means that the pack requires a
   ``common::bits`` pack up to 2.1. This includes all previous versions
   and also all patches for version 2.1 (e.g. 2.1.7, 2.1.8, ...) but not
   version 2.2 or newer.

-  ``Registry::Pack @< Version`` - the pack requires a dependency with
   version older than the specified one. For example,
   ``common::bits @< 3`` means that the pack requires a ``common::bits``
   2.x or older version.

-  ``Registry::Pack @> Version`` - the pack requires a dependency with
   version newer than the specified one. For example,
   ``common::bits @> 2:4`` means that the pack requires a
   ``common::bits`` 2.5 or newer version.

-  ``Registry::Pack == Version`` - the pack requires a dependency with a
   specific version. For example, ``common::bits == 2:1`` means that the
   pack requires a ``common::bits`` pack version 2.1.x (thus, from
   version 2.1.0 to the latest patch for version 2.1).

-  ``Registry::Pack \== Version`` - the pack requires a dependency with
   any version other than then the one specified. For example,
   ``common::bits \== 2.1`` means that the pack requires a
   ``common::bits`` pack version other than any 2.1.x version.

It's also possible to specify *range* dependencies by using two
consecutive elements with the lower bound followed by the upper bound.
For example, ``[common::bits @>= 2, common::bits @< 3]`` means all
``common::bits`` 2.x versions but not older or newer major versions.

Pack portability
----------------

Ideally, packs are fully portable and can be used with all Logtalk
supported Prolog backends. This can be declared by using the atom
``all`` in the last argument of the ``version/6`` predicate (see example
above).

When a pack can only be used with a subset of the Prolog backends, the
last argument of the ``version/6`` predicate is a list of backend
identifiers (atoms):

-  B-Prolog: ``b``
-  Ciao Prolog: ``ciao``
-  CxProlog: ``cx``
-  ECLiPSe: ``eclipse``
-  GNU Prolog: ``gnu``
-  JIProlog: ``ji``
-  LVM: ``lvm``
-  Scryer Prolog: ``scryer``
-  SICStus Prolog: ``sicstus``
-  SWI-Prolog: ``swi``
-  Tau Prolog: ``tau``
-  Trealla Prolog: ``trealla``
-  XSB: ``xsb``
-  YAP: ``yap``

Pack development
----------------

To simplify pack development and testing, define a local registry and
add to it a pack specification with the development version available
from a local directory. For example:

::

   version(
       0:11:0,
       beta,
       'file:///home/jdoe/work/my_awesome_library',
       none,
       [],
       all
   ).

If the directory is a git repo, the tool will clone it when installing
the pack. Otherwise, the files in the directory are copied to the pack
installation directory. This allows the pack to be installed, updated,
and uninstalled without consequences for the pack source files.

Packs that are expected to be fully portable should always be checked by
loading them with the ``portability`` flag set to ``warning``.

To check your packs specifications, use the ``packs::lint/0-2``
predicates after adding the registry that provides the packs.

Pack handling
-------------

Packs must be available from a defined registry. To list all packs that
are available for installation, use the ``packs::available/0``
predicate:

::

   | ?- packs::available.

To list all installed packs, call the ``packs::installed/0`` predicate:

::

   | ?- packs::installed.

To know more about a specific pack, use the ``packs::describe/1-2``
predicates. For example:

::

   | ?- packs::describe(bar).

The ``packs::describe/2`` predicate can be used when two or more
registries provide packs with the same name. For example:

::

   | ?- packs::describe(reg, bar).

To install the latest version of a pack, we can use the
``packs::install/1-4`` predicates. In the most simple case, when a pack
name is unique among registries, we can use the ``packs::install/1``
predicate. For example:

::

   | ?- packs::install(bar).

Any pack dependencies are also checked and installed or updated if
necessary. Other install predicates are available to disambiguate
between registries and to install a specific pack version.

Packs becomes available for loading immediately after successful
installation (no restarting of the Logtalk session is required). For
example, after the pack ``bar`` is installed, you can load it at the
top-level by typing:

::

   | ?- {bar(loader)}.

or load it from a loader file using the goal
``logtalk_load(bar(loader))``.

After updating the defined registries, outdated packs can be listed
using the ``packs::outdated/0`` predicate. You can update all outdated
packs by calling the ``packs::update/0`` predicate or update a single
pack using the ``packs::update/1-2`` predicates. For example:

::

   | ?- packs::update(bar).

The tool provides versions of the pack install, update, and uninstall
predicates that accept a list of options:

-  ``verbose(Boolean)`` (default is ``false``)
-  ``clean(Boolean)`` (default is ``false``)
-  ``force(Boolean)`` (default is ``false``)
-  ``checksum(Boolean)`` (default is ``true``)
-  ``checksig(Boolean)`` (default is ``false``)
-  ``curl(Atom)`` (extra command-line options; default is ``''``)
-  ``gpg(Atom)`` (extra command-line options; default is ``''``)
-  ``tar(Atom)`` (extra command-line options; default is ``''``)

When using a ``checksig(true)`` option to check a pack signature, is
strongly advised that you also use the ``verbose(true)`` option. For
example:

::

   | ?- packs::install(foo, bar, 1:1:2, [verbose(true), checksig(true)]).

Note that the public key used to sign the pack archive must be already
present in your local system.

Downloading pack archives may require passing extra command-line options
to ``curl`` for authentication. A common solution is to use a personal
access token. The details depend on the server software. For example:

::

   | ?- packs::install(foo, bar, 1:1:2, [curl('--header "Authorization: token foo42"')]).

Or:

::

   | ?- packs::install(foo, bar, 1:1:2, [curl('--header "PRIVATE-TOKEN: foo42"')]).

Pack archives may be encrypted, requiring passing the decryption
passphrase when installing or updating a pack. For example:

::

   | ?- packs::install(foo, bar, 1:1:2, [tar('--passphrase test123')]).

In this case, you should be careful to not leak your passphrase in e.g.
the query history.

To uninstall a pack that you no longer need, use the
``packs::uninstall/1-2`` predicates. By default, only packs with no
dependent packs can be uninstalled. You can print or get a list of the
packs that depend on a given pack by using the ``packs::dependents/1-3``
predicates. For example:

::

   | ?- packs::dependents(reg, bar, Dependents).

See the tool API documentation on the
`packs <../../docs/packs_0.html>`__ object for other useful predicates.

Pack documentation
------------------

The path to the pack ``README.md`` file is printed when the pack is
installed or updated. It can also be retrieved at any time by using the
``readme/2`` predicate. For example:

::

   | ?- packs::readme(lflat, Path).

Additional documentation may also be available from the pack home page,
which can be printed by using the ``describe/1-2`` predicates. For
example:

::

   | ?- packs::describe(lflat).

   % Registry:    ...
   % Pack:        lflat
   % Description: L-FLAT - Logtalk Formal Language and Automata Toolkit
   % License:     MIT
   % Home:        https://github.com/l-flat/lflat
   % Versions:
   ...

The pack API documentation can be generated using the ``lgtdoc`` tool
library and directory predicates (depending on the pack source files
organization). For example:

::

   | ?- {lflat(loader)},
        {lgtdoc(loader)},
        logtalk::expand_library_path(lflat, Path),
        lgtdoc::rdirectory(Path).
   ...

This query creates a ``xml_docs`` directory in the current directory.
The XML documentation files can then be converted into a final format,
e.g. HTML, using one of the ``lgtdoc`` tool provided scripts. For
example:

::

   $ cd xml_docs
   $ lgt2html

For more details and alternatives, see the ``lgtdoc`` tool
documentation.

It is also possible to add API documentation and diagrams for all the
installed packs to the Logtalk distribution API documentation and
diagrams by calling the ``update_html_docs`` and ``update_svg_diagrams``
scripts with the ``-i`` option. See the scripts documentation for more
details.

Pinning registries and packs
----------------------------

Registries and packs can be *pinned* after installation to prevent
accidental updating or deleting, e.g. when using the batch ``update/0``
predicate. This is useful when your application requires a specific
version or for security considerations (see below). For example, if we
want the ``bar`` pack to stay at its current installed version:

::

   | ?- packs::pin(bar).
   yes

After, any attempt to update or uninstall the pack will fail with an
error message:

::

   | ?- packs::update(bar).
   !     Cannot update pinned pack: bar
   no

   | ?- packs::uninstall(bar).
   !     Cannot uninstall pinned pack: bar
   no

To enable the pack to be updated ou uninstalled, the pack must first be
unpinned. Alternatively, the ``force(true)`` option can be used. Note
that if you force update a pinned pack, the new version will be
unpinned.

It's also possible to pin (or unpin) all defined registries or installed
packs at once by using the ``pin/0`` (or ``unpin/0``) predicates. But
note that registries added after or packs installed after will not be
automatically pinned.

Testing packs
-------------

Logtalk packs (as most Logtalk libraries, tools, and examples) are
expected to have a ``tester.lgt`` or ``tester.logtalk`` tests driver
file at the root of their directory, which can be used for both
automated and manual testing. For example, after installing the ``foo``
pack:

::

   | ?- {foo(tester)}.

To test all installed packs, you can use the ``logtalk_tester``
automation script from the installed packs directory, which you can
query using the goal:

::

   | ?- packs::prefix(Directory).

Note that running the packs tests, like simply loading the pack, can
result in calling arbitrary code, which can potentially harm your
system. Always take into account the security considerations discussed
below.

Security considerations
-----------------------

New pack registries should be examined before being added, specially if
public and from a previously unknown source. The same precautions should
be taken when adding or updating a pack. Note that a registry can always
index third-party packs.

Pack checksums are checked by default. But pack signatures are only
checked if requested as packs are often unsigned. Care should be taken
when adding public keys for pack signers to your local system. Detached
signature files are assumed and expected to share the name of the
archive and use a ``.asc`` extension. When the ``checksig(true)`` option
is used, the signature file is automatically downloaded using a URL
constructed from the pack archive URL.

Registry and pack spec files plus the registry loader file are compiled
by term-expanding them so that only expected terms are actually loaded
and only expected ``logtalk_load/2`` goals with expected relative file
paths are allowed. Predicates defining URLs are discarded if the URLs
are neither ``https://`` nor ``file://`` URLs or if they contain
non-allowed characters (currently, only alpha-numeric ASCII characters
plus the ASCII ``/``, ``.``, ``-``, and ``_`` characters are accepted).
But note that this tool makes no attempt to audit pack source files
themselves.

Registries and packs can always be pinned so that they are not
accidentally updated to a version that you may not had the chance to
audit.

Best practices
--------------

-  Make available a new pack registry as a git repo. This simplifies
   updating the registry and rolling back to a previous version.

-  Use registry and pack names that are valid unquoted atoms, thus
   simplifying usage. Use descriptive names with underscores if
   necessary to link words.

-  Name registry and pack specification objects after their names with a
   ``_registry`` or ``_pack`` suffix. Save the objects in files named
   after the objects.

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

Installing Prolog packs
-----------------------

This tool can also be used to install Prolog packs that don't use
Logtalk. After installing a ``pl_pack`` Prolog pack from a ``pl_reg``
registry, it can be found in the ``$LOGTALKPACKS/packs/pl_reg/pl_pack``
directory. When the ``LOGTALKPACKS`` environment variable is not
defined, the pack directory is by default
``~/logtalk_packs/packs/pl_reg/pl_pack``.

Different Prolog systems provide different solutions for locating Prolog
code. For example, several Prolog systems adopted the Quintus Prolog
``file_search_path/2`` hook predicate. For these systems, a solution
could be to add a fact to this predicate for each installed Prolog pack.
For example, assuming a ``pl_pack`` Prolog pack:

::

   :- multifile(file_search_path/2).
   :- dynamic(file_search_path/2).

   file_search_path(library, '$LOGTALKPACKS/packs/pl_pack').

If the Prolog system also supports reading an initialization file at
startup, the above definition could be added there.

Known issues
------------

Using the ``verbose(true)`` option on Windows systems may not provide
the shell commands output depending on the backend.

On Windows systems, the reset, delete, and uninstall predicates may fail
to delete all affected folders and files due to a operating-system bug.
Depending on the backend, this bug may cause some of the tests to fail.
For details on this bug, see:

https://github.com/microsoft/terminal/issues/309

The workaround is to use the Windows File Explorer to delete the
left-over folders and files.

When using Ciao Prolog 1.20.0, a workaround is used for this system
non-standard support for multifile predicates.

When using GNU Prolog 1.5.0 as the backend on Windows, you may get an
error on ``directory_files/2`` calls. For details and a workaround, see:

https://github.com/didoudiaz/gprolog/issues/4

This issue is fixed in the GNU Prolog 1.5.1 version.

Using SICStus Prolog as the backend on Windows doesn't currently work in
version 4.7.0 and earlier versions. The underlying issues are fixed in
the SICStus Prolog 4.7.1 version.

XSB have an odd bug (likely in its parser) when reading files that may
cause a pack installed version to be reported as the ``end_of_file``
atom.

Some tests fail on Windows when using ECLiPSe or XSB due to file path
representation issues.
