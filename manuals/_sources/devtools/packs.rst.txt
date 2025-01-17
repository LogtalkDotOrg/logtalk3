.. _library_packs:

``packs``
=========

This tool provides predicates for downloading, installing, upgrading,
and uninstalling third-party libraries and applications, generically
known as *packs*. Collections of pack specifications are made available
using *registries*. Registries can be local to a system, publicly
shared, or private to a company (e.g., only available in a VPN). There
is no concept of a central registry. Users can decide which registries
they trust and want to use and add them using their published URLs. The
tool supports both pack checksums and signatures and takes several steps
to sanitize registry and pack specifications. As with other Logtalk
developer tools, portability is a main goal. This tool can be used with
any supported Prolog backend and run on both POSIX and Windows systems.
Moreover, this tool can be used not only for handling Logtalk packs but
also for Prolog only packs, thus providing a solution for sharing
portable resources between multiple systems.

A list of public Logtalk and Prolog pack registries is available at:

https://github.com/LogtalkDotOrg/pack-registries

This tool is in the beta stage of development. Feedback is most welcome.

Requirements
------------

On POSIX systems (Linux, macOS, ...), the following shell commands are
required:

- ``sha256sum`` (provided by GNU ``coreutils``)
- ``curl`` (default file downloader)
- ``wget`` (alternative file downloader)
- ``bsdtar`` (provided by ``libarchive`` or ``libarchive-tools``)
- ``gpg`` (provided by ``gnupg2``)
- ``git``
- ``direnv`` (when using virtual environments)

The tool uses ``bsdtar`` instead of GNU ``tar`` so that it can
uncompress ``.zip`` archives (``unzip`` doesn't provide the desired
options that allow a simple and reliable solution for ignoring the
non-predictable name of the wrapper directory).

On Windows systems, the following shell commands are required:

- ``certutil.exe``
- ``curl.exe`` (default file downloader)
- ``wget.exe`` (alternative file downloader)
- ``tar.exe``
- ``gpg.exe``
- ``git.exe``
- ``Set-PsEnv`` (when using virtual environments)

In recent Windows 10 builds, only ``wget``, ``gpg``, ``git``, and
``Set-PsEnv`` should require installation. You can download the GnuPG
software from:

https://www.gnupg.org/

You can download Git from:

https://gitforwindows.org

You can download Wget from:

https://eternallybored.org/misc/wget/

You can also use Chocolatey to install the commands above:

::

   > choco install gnupg git wget

To install `Set-PsEnv <https://github.com/rajivharris/Set-PsEnv>`__ from
the PowerShell Gallery:

::

   PS> Install-Module -Name Set-PsEnv

On macOS systems, Apple bundles both ``curl`` and BSD ``tar`` (under the
name ``tar``; you can simply create a ``bsdtar`` alias or install a more
recent version). The required commands can be easily installed using
MacPorts:

::

   $ sudo port install coreutils wget libarchive gnupg2 git direnv

Or using Homebrew:

::

   $ brew install coreutils wget libarchive gnupg2 git direnv

On Linux systems, use the distribution's own package manager to install
any missing command. For example, in recent Ubuntu versions:

::

   $ sudo apt update
   $ sudo apt install coreutils curl wget libarchive-tools gnupg2 git direnv

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
defined. Otherwise, it defaults to the ``~/logtalk_packs`` directory.
The actual directory can be retrieved by the query:

::

   | ?- packs::logtalk_packs(Directory).
   ...

This directory holds sub-directories for registries, packs, and
archives. These sub-directories are automatically created when loading
the ``packs`` tool if they don't exist. Users should not manually modify
the contents of these directories. Multiple and independent
registry/pack setups are possible using *virtual environments* as
explained next.

Your registries and packs setup can be saved and restored (e.g., in a
different system) by using the ``packs::save/1-2`` and
``packs::restore/1-2`` predicates, as explained in the next section
about virtual environments. If necessary, before restoring, the
``packs::reset/0`` predicate can be called to delete any defined
registries and installed packs.

Virtual environments
--------------------

An application may require specific pack versions. These requirements
may differ between applications. Different applications may also have
conflicting requirements. Therefore, a *virtual environment* where an
application requirements are fulfilled may be required to develop and/or
run it. A virtual environment is essentially a registries/packs storage
directory.

Defining the ``logtalk_packs`` library alias in a settings file or
defining the ``LOGTALKPACKS`` environment variable before starting
Logtalk allows easy creation and switching between virtual environments.
By using a per-application settings file (or a per-application
environment variable definition), each application can thus use its own
virtual environment. The ``settings.lgt`` file can define the
``logtalk_packs`` library alias using code such as:

::

   :- initialization((
       logtalk_load_context(directory, Directory),
       assertz(logtalk_library_path(logtalk_packs, Directory))
   )).

The definition of the ``logtalk_packs`` library alias **must** always be
an atom and thus never use library notation (i.e., it must never depend
on other library aliases).

When a virtual environment also requires a specific Logtalk version
(e.g., the version used to test and certify it), this can be installed
as a pack from the official
`talkshow <https://github.com/LogtalkDotOrg/talkshow>`__ registry and
used by (re)defining the ``LOGTALKHOME`` and ``LOGTALKUSER`` environment
variables to point to its pack directory (which can be queried by using
the ``packs::directory/2`` message).

Experimental ``lgtenv.sh`` and ``lgtenv.ps1`` scripts are included to
simplify creating virtual environments. For example:

::

   $ lgtenv -d ~/my_venv -c -p logtalk_packs
   $ cd ~/my_venv
   direnv: loading ~/my_venv/.envrc
   direnv: export +LOGTALKPACKS

Type ``lgtenv -h`` for details on the script options.

These scripts require, respectively,
`direnv <https://github.com/direnv/direnv>`__ and
`Set-PsEnv <https://github.com/rajivharris/Set-PsEnv>`__ to be
installed. These utilities load and unload environment variables when
changing the current directory. On Windows systems, when using the
``lgtenv.ps1`` script, you also need to redefine the PowerShell prompt
in a profile file (e.g., ``$HOME\Documents\PowerShell\Profile.ps1``) to
mimic the functionality of ``direnv`` of automatically loading an
existing ``.env`` file when changing to its directory. For example:

::

   function prompt {
       Set-PsEnv
       'PS ' + $(Get-Location) + '> '
   }

A virtual environment setup (i.e., the currently defined registries and
installed packs) can be saved into a file (e.g., ``requirements.lgt``)
using the ``packs::save/1`` predicate:

::

   | ?- packs::save('requirements.lgt').
   ...

This query saves a listing of all the installed packs and their
registries. Using the saved file, the virtual environment setup can then
be restored using the ``packs::restore/1-2`` predicates. The file uses a
simple format with ``registry/2``, ``pack/3``, ``pinned_registry/1``,
and ``pinned_pack/1`` facts (in this order) and can be manually created
or edited if necessary. For example:

::

   registry(talkshow, 'https://github.com/LogtalkDotOrg/talkshow.git').
   pack(talkshow, logtalk, 3:45:0).
   pack(talkshow, lflat, 2:1:0).

These files can be distributed with applications so that users can
easily fulfill application requirements by running the query once:

::

   | ?- packs::restore('requirements.lgt').

Subsequently, the application ``loader.lgt`` file can then load the
required packs using their loader files:

::

   :- initialization((
       % load required packs
       logtalk_load(foo(loader)),
       logtalk_load(bar(loader)),
       ...
       % load application files
       ...
   )).

Note that restoring encrypted registries or encrypted packs requires
entering the required passphrases. Although the ``restore/2`` predicate
accepts a list of options that include the ``gpg/1`` option, this only
allows specifying a single and common passphrase when interactive
entering of passphrases is not convenient or possible.

Registry specification
----------------------

A registry is a git remote repo that can be cloned, a downloadable or
local archive, or a local directory containing a Logtalk loader file
that loads source files defining the registry itself and the packs it
provides. The registry name is ideally a valid unquoted atom. The
registry directory must contain at least two Logtalk source files:

- A file defining an object named after the registry with a
  ``_registry`` suffix, implementing the ``registry_protocol``. This
  naming convention helps prevent name conflicts.

- A loader file (named ``loader.lgt`` or ``loader.logtalk``) that loads
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
also be queried using the ``registries::directory/2`` predicate. The
``NOTES.md`` file name can also be used in alternative to the
recommended ``README.md`` file name.

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

It would be, of course, possible to have all objects in a single source
file. But having a file per-object and a loader file helps maintenance,
and it's also a tool requirement for applying safety procedures to the
source file contents and thus successfully loading the registry and pack
specs.

As registries are git repos in the most common case, and thus adding
them performs a git repo cloning, they should only contain the strictly
required files.

Registry handling
-----------------

Registries can be added using the ``registries::add/1-3`` predicates,
which take a registry URL. Using the example above:

::

   | ?- registries::add('https://github.com/jdoe/jdoe_awesome_packs.git').

HTTPS URLs must end with either a ``.git`` extension or an archive
extension (same valid extensions as for pack archives, including ``gpg``
encrypted). Git cloning URLs are preferred as they simplify updating
registries. But a registry can also be made available via a local
directory (using a ``file://`` URL) or a downloadable archive (using a
``https://`` URL).

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

When a registry may be already defined, you can use the ``update(true)``
option to ensure that the registry will be updated to its latest
definition:

::

   | ?- registries::add(
           jdoe_awesome_packs,
           'https://github.com/jdoe/jdoe_awesome_packs/archive/main.zip',
           [update(true)]
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
the pack with a ``_pack`` suffix. This naming convention helps prevent
name conflicts, notably with the pack's own objects. The file must be
available from a declared pack registry (by having the registry loader
file loading it). The pack name is preferably a valid unquoted atom. An
example of a pack specification object would be:

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
           [logtalk @>= 3:42:0],
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

The ``license/1`` argument must be an atom and should, whenever
possible, be a license identifier as specified in the `SPDX
standard <https://spdx.org/licenses/>`__.

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

- ``.zip``
- ``.tgz``, ``.tar.gz``
- ``.tbz2``, ``.tar.bz2``

Also, for encrypted packs, all the extensions above with a ``.gpg``
suffix (e.g., ``.zip.gpg``).

The pack sources should contain ``LICENSE``, ``README.md`` (or
``NOTES.md``), and ``loader.lgt`` (or ``loader.logtalk``) files.
Ideally, it should also contain a ``tester.lgt`` (``tester.logtalk``)
file. The path to the ``README.md`` file is printed when the pack is
installed or updated. It can also be queried using the
``packs::directory/2`` predicate.

Encrypted packs
---------------

Packs can be ``gpg`` encrypted, with a choice of passphrase-based
encryption, key-based encryption, or both. Encrypted pack archives must
always have a ``.gpg`` extension. For example, to encrypt a pack archive
with a symmetric cipher using a passphrase:

::

   $ tar -cvzf - my_pack | gpg -c --cipher-algo AES256 > v1.2.1.tar.gz.gpg

In this case, the passphrase would need to be securely communicated to
any users installing or updating the pack.

See the ``gpg`` documentation for full details on encrypting and
decrypting archives. If you get a "gpg: problem with the agent:
Inappropriate ioctl for device" error message with the command above,
try:

::

   $ export GPG_TTY=$(tty)

Signed packs
------------

Packs can be ``gpg`` signed. Detached signature files are assumed and
expected to share the name of the archive and use ``.asc`` or ``.sig``
extensions. For example, if the pack archive name is ``v1.0.0.tar.gz``,
the signature file must be named ``v1.0.0.tar.gz.asc`` or
``v1.0.0.tar.gz.sig``. When the ``checksig(true)`` option is used, the
signature file is automatically downloaded using a URL constructed from
the pack archive URL. When both ``.asc`` and ``.sig`` files exist, the
``.asc`` file is used. An example of signing a pack and creating the
``.asc`` file (assuming the default signing key) is:

::

   $ gpg --armor --detach-sign v1.0.0.tar.gz

To create instead a ``.sig`` file:

::

   $ gpg --detach-sign v1.0.0.tar.gz

See the ``gpg`` documentation for full details on signing archives and
sharing the public keys required to verify the signatures.

Pack URLs and Single Sign-On
----------------------------

Typically, pack archive download URLs are HTTPS URLs and handled using
``curl``. It's also possible to use ``git archive`` to download pack
archives, provided that the server supports it (as of this writing,
Bitbucket and GitLab public hosting services support it but not GitHub).
Using ``git archive`` is specially useful when the packs registry is
hosted on a server using Single Sign-On (SSO) for authentication. In
this case, HTTPS URLs can only be handled by ``curl`` by passing a token
(see below for an example). When the user has setup SSH keys to
authenticate to the packs registry server, ``git archive`` simplifies
pack installation, providing a better user experience. For example:

::

   version(
       1:0:1,
       stable,
       'git@gitlab.com:me/foo.git/v1.0.1.zip',
       sha256 - '0894c7cdb8968b6bbcf00e3673c1c16cfa98232573af30ceddda207b20a7a207',
       [logtalk @>= 3:36:0],
       all
   ).

The pseudo-URL must be the concatenation of the SSH repo cloning URL
with the archive name. The archive name must be the concatenation of a
valid tag with a supported archive extension. SSH repo cloning URLs use
the format:

::

   git@<hostname>:path/to/project.git

They can usually be easily copied from the hosting service repo webpage.
To compute the checksum, you must first download the archive. For
example:

::

   $ git archive --output=foo-v1.0.1.zip --remote=git@gitlab.com:me/foo.git v1.0.1
   $ openssl sha256 foo-v1.0.1.zip

Be sure to use a format that is supported by both the ``packs`` tool and
the ``git archive`` command (the format is inferred from the
``--output`` option). Do not download the archive from the web interface
of the git hosting service in order to compute the checksum. Different
implementations of the archiving and compressing algorithms may be used,
resulting in mismatched checksums.

Users installing packs available using ``git archive`` URLs are advised
to run a SSH agent to avoid being prompted for passwords when installing
or updating packs. They must also upload their SSH public keys to the
pack provider hosts.

Multiple pack versions
----------------------

A pack may specify multiple versions. Each version is described using a
``version/6`` predicate clause as illustrated in the example above. The
versions must be listed in order from newest to oldest. For details, see
the ``pack_protocol`` API documentation.

Listing multiple versions allows the pack specification to be updated
(by updating its registry) without forcing existing users into
installing (or updating to) the latest version of the pack.

Pack dependencies
-----------------

Pack dependencies on other packs can be specified using a list of
``Registry::Pack Operator Version`` terms where ``Operator`` is a
standard term comparison operator:

- ``Registry::Pack @>= Version`` - the pack requires a dependency with a
  version equal or above the specified one. For example,
  ``logtalk @>= 3:36:0`` means that the pack requires Logtalk 3.36.0 or
  a later version.

- ``Registry::Pack @=< Version`` - the pack requires a dependency with a
  version up to the specified one. For example, ``common::bits @=< 2:1``
  means that the pack requires a ``common::bits`` pack up to 2.1. This
  includes all previous versions and also all patches for version 2.1
  (e.g., 2.1.7, 2.1.8, ...) but not version 2.2 or newer.

- ``Registry::Pack @< Version`` - the pack requires a dependency with
  version older than the specified one. For example,
  ``common::bits @< 3`` means that the pack requires a ``common::bits``
  2.x or older version.

- ``Registry::Pack @> Version`` - the pack requires a dependency with
  version newer than the specified one. For example,
  ``common::bits @> 2:4`` means that the pack requires a
  ``common::bits`` 2.5 or newer version.

- ``Registry::Pack == Version`` - the pack requires a dependency with a
  specific version. For example, ``common::bits == 2:1`` means that the
  pack requires a ``common::bits`` pack version 2.1.x (thus, from
  version 2.1.0 to the latest patch for version 2.1).

- ``Registry::Pack \== Version`` - the pack requires a dependency with
  any version other than the one specified. For example,
  ``common::bits \== 2.1`` means that the pack requires a
  ``common::bits`` pack version other than any 2.1.x version.

To specify *range* dependencies by using two consecutive elements with
the lower bound followed by the upper bound. For example,
``common::bits @>= 2, common::bits @< 3`` means all ``common::bits`` 2.x
versions but not older or newer major versions.

It's also possible to specify *alternative* dependencies using the
``(;)/2`` operator. For example,
``(common::bits == 1:9; common::bits @>= 2:3)`` means either
``common::bits`` 1.9.x versions or 2.3.x and later versions.
Alternatives should be listed in decreasing order of preference.

When a pack also depends on a Logtalk or backend version, the name
``logtalk`` or the backend identifier atom can be used in place of
``Registry::Pack`` (see below for the table of backend specifiers). For
example, ``logtalk @>= 3.36.0``.

When a pack also depends on an operating-system version (e.g., a pack
containing shared libraries with executable code), the
``os(Name,Machine)`` compound term can also be used in place of
``Registry::Pack``. For example, ``os('Darwin',x86_64) @>= '23.0.0'``.
Note that, in this case, the release is an atom. The operating-system
data (name, machine, and release) is queried using the corresponding
``os`` library predicates (see the library documentation for details).

Pack portability
----------------

Ideally, packs are fully portable and can be used with all
Logtalk-supported Prolog backends. This can be declared by using the
atom ``all`` in the last argument of the ``version/6`` predicate (see
example above).

When a pack can only be used with a subset of the Prolog backends, the
last argument of the ``version/6`` predicate is a list of backend
identifiers (atoms):

- B-Prolog: ``b``
- Ciao Prolog: ``ciao``
- CxProlog: ``cx``
- ECLiPSe: ``eclipse``
- GNU Prolog: ``gnu``
- JIProlog: ``ji``
- XVM: ``xvm``
- Quintus Prolog: ``quintus``
- SICStus Prolog: ``sicstus``
- SWI-Prolog: ``swi``
- Tau Prolog: ``tau``
- Trealla Prolog: ``trealla``
- XSB: ``xsb``
- YAP: ``yap``

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

You can also use a local archive instead of a directory. For example:

::

   version(
       1:0:0,
       stable,
       'file:///home/jdoe/work/my_awesome_library/v1.0.0.tar.gz',
       sha256 - '1944773afba1908cc6194297ff6b5ac649a844ef69a69b2bcdf267cfa8bfce1e',
       [],
       all
   ).

Packs that are expected to be fully portable should always be checked by
loading them with the ``portability`` flag set to ``warning``.

To check your pack manifest files, use the ``packs::lint/0-2``
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

To list only the installed packs from a specific registry, call instead
the ``packs::installed/1`` predicate. For example:

::

   | ?- packs::installed(talkshow).

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

Packs become available for loading immediately after successful
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

By default, updating a pack fails if it would break any dependent pack
(the ``force(true)`` option, described below, can be used to force
updating in this case).

The tool provides versions of the pack install, update, and uninstall
predicates that accept a list of options:

- ``verbose(Boolean)`` (default is ``false``)
- ``clean(Boolean)`` (default is ``false``)
- ``update(Boolean)`` (default is ``false``)
- ``force(Boolean)`` (default is ``false``)
- ``compatible(Boolean)`` (default is ``true``)
- ``checksum(Boolean)`` (default is ``true``)
- ``checksig(Boolean)`` (default is ``false``)
- ``git(Atom)`` (extra command-line options; default is ``''``)
- ``downloader(Atom)`` (downloader utility; default is ``curl``)
- ``curl(Atom)`` (extra command-line options; default is ``''``)
- ``wget(Atom)`` (extra command-line options; default is ``''``)
- ``gpg(Atom)`` (extra command-line options; default is ``''``)
- ``tar(Atom)`` (extra command-line options; default is ``''``)

Note that, by default, only compatible packs can be installed. To
install a pack that is incompatible with the current Logtalk version,
backend version, or operating-system version, use the ``install/4`` or
``update/3`` predicates with the option ``compatible(false)``.

When installing large packs over unreliable network conditions, you may
try switching the default downloader utility from ``curl`` to ``wget``.

When a pack may already be installed, you can use the ``update(true)``
option to ensure that the installation will be updated to the specified
version:

::

   | ?- packs::install(reg, bar, 1:1:2, [update(true)]).

When using a ``checksig(true)`` option to check a pack signature, it is
strongly advised that you also use the ``verbose(true)`` option. For
example:

::

   | ?- packs::install(reg, bar, 1:1:2, [verbose(true), checksig(true)]).

Note that the public key used to sign the pack archive must already be
present in your local system.

Downloading pack archives may require passing extra command-line options
to ``curl`` for authentication. A common solution is to use a personal
access token. The details depend on the server software. An example when
using GitHub:

::

   | ?- packs::install(reg, bar, 1:1:2, [curl('--header "Authorization: token foo42"')]).

Another example when using GitLab:

::

   | ?- packs::install(reg, bar, 1:1:2, [curl('--header "PRIVATE-TOKEN: foo42"')]).

Pack archives may be ``gpg`` encrypted. Encryption can be
passphrase-based, key-based, or both. When using only passphrase-based
encryption, the archive passphrase must be entered (if not cached) when
installing or updating a pack. In this case, the passphrase can be
entered interactively or using the ``gpg/1`` option. For example:

::

   | ?- packs::install(reg, bar, 1:1:2, [gpg('--batch --passphrase test123')]).

See the ``gpg`` documentation for details. When using the ``gpg/1``
option, you should be careful to not leak passphrases in, e.g., the
query history.

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
e.g., HTML, using one of the scripts provided by the ``lgtdoc`` tool.
For example:

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
accidental updating or deleting, e.g., when using the batch ``update/0``
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

To enable the pack to be updated or uninstalled, the pack must first be
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
checked if requested, as packs are often unsigned. Care should be taken
when adding public keys for pack signers to your local system.

Registry and pack spec files, plus the registry loader file, are
compiled by term-expanding them so that only expected terms are actually
loaded and only expected ``logtalk_load/2`` goals with expected relative
file paths are allowed. Predicates defining URLs are discarded if the
URLs are neither ``https://`` nor ``file://`` URLs or if they contain
non-allowed characters (currently, only alpha-numeric ASCII characters
plus the ASCII ``/``, ``.``, ``-``, and ``_`` characters are accepted).
But note that this tool makes no attempt to audit pack source files
themselves.

Registries and packs can always be pinned so that they are not
accidentally updated to a version that you may not have had the chance
to audit.

Best practices
--------------

- Make available a new pack registry as a git repo. This simplifies
  updating the registry and rolling back to a previous version.

- Use registry and pack names that are valid unquoted atoms, thus
  simplifying usage. Use descriptive names with underscores if necessary
  to link words.

- Name the registry and pack specification objects after their names
  with a ``_registry`` or ``_pack`` suffix. Save the objects in files
  named after the objects.

- Create new pack versions from git tags.

- If the sources of a pack are available from a git repo, consider using
  signed commits and signed tags for increased security.

- When a new pack version breaks backwards compatibility, list both the
  old and the new versions on the pack specification file.

- Pin registries and packs when specific versions are critical for your
  work so that you can still easily batch update the remaining packs and
  registries.

- Include the ``$LOGTALKPACKS`` directory (or the default
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

Help with warnings
------------------

Load the ``tutor`` tool to get help with selected warnings printed by
the ``packs`` tool.

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
leftover folders and files.

When using Ciao Prolog 1.20.0, a workaround is used for this system
non-standard support for multifile predicates.

When using GNU Prolog 1.5.0 as the backend on Windows, you may get an
error on ``directory_files/2`` calls. For details and a workaround, see:

https://github.com/didoudiaz/gprolog/issues/4

This issue is fixed in the latest GNU Prolog git version.

Using SICStus Prolog as the backend on Windows doesn't currently work in
version 4.7.0 and earlier versions. The underlying issues are fixed in
the SICStus Prolog 4.7.1 version.

XSB has an odd bug (likely in its parser) when reading files that may
cause a pack installed version to be reported as the ``end_of_file``
atom.

Some tests fail on Windows when using ECLiPSe or XSB due to file path
representation issues.
