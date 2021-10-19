________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


``packs``
=========

This tool provides predicates for downloading, installing, upgrading, and
uninstalling third-party libraries and applications, generically known as
_packs_. Collections of pack specifications are made available using
_registries_. Registries can be local to a system, publicly shared, or
private to a company VPN. There is no concept of a central registry. Users
decide which registries they trust and want to use and add them using their
published URLs. The tool supports both pack checksums and signatures and
takes several steps to sanitize registry and pack specifications. As other
Logtalk developer tools, portability is main goal. This tool can be used
with any supported Prolog backend and run in both POSIX and Windows systems.
Moreover, this tool can be used not only for handling Logtalk packs but also
Prolog only packs, thus providing a stepping stone for sharing portable
resources between multiple systems.


Requirements
------------

On POSIX systems (Linux, macOS, ...), the following shell commands are
required:

- `sha256sum` (provided by GNU `coreutils`)
- `curl`
- `bsdtar` (provided by `libarchive`)
- `gpg` (provided by `gnupg2`)

The tool uses `bsdtar` instead of GNU `tar` so that it can uncompress
`.zip` archives (`unzip` doesn't provide the desired options). The `gpg`
command is only required if you want to check pack archive signatures.

On Windows systems, the following shell commands are required:

- `certutil.exe`
- `curl.exe`
- `tar.exe`
- `gpg`

In recent Windows 10 builds, only `gpg` should require installation. You
can download the GnuPG software from:

https://www.gnupg.org/

On macOS systems, Apple bundles both `curl` and BSD `tar` (under the name
`tar`; you can simply create a `bsdtar` alias or install a more recent
version). The required commands can be easily installed using MacPorts:

	$ sudo port install coreutils libarchive gnupg2

Or using Homebrew:

	$ brew install coretutils libarchive gnupg2

On Linux systems, use the distribution own package manager to install any
missing command.


API documentation
-----------------

This tool API documentation is available at:

[../../docs/library_index.html#packs](../../docs/library_index.html#packs)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(packs(loader)).


Testing
-------

To test this tool, load the `tester.lgt` file:

	| ?- logtalk_load(packs(tester)).


Usage
-----

The tool provides two main objects, `registries` and `packs`, for handling,
respectively, registries and packs. Both objects accept a `help/0` message.


Registries and packs storage
----------------------------

The tool uses a directory specified using the `LOGTALKPACKS` environment
variable when defined. Otherwise it defaults to `~/logtalk_packs`. This
directory holds sub-directories for registries, packs, and archives. Users
shouldn't manually modify the contents of these directories.

The `packs` tools loads at startup defined registry and pack specifications.
Therefore, using the `LOGTALKPACKS` environment variable allows easy switching
between alternative registry/pack setups by simply changing the value of the
variable before loading the tool.


Registry specification
----------------------

A registry is a git remote repo that can be cloned, a downloadable archive,
or a local directory containing a Logtalk loader file that loads source files
defining the registry itself and the packs it provides. The registry name is
ideally a valid unquoted atom. The registry directory must contain at least
two Logtalk source files:

- A file defining an object named after the registry with a `_registry`
suffix, implementing the `registry_protocol`.

- A loader file (named `loader.lgt` or `loader.logtalk`) that loads the
registry object file and all pack object files.

An example of a registry specification object would be:

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

The registry directory should also contain `LICENSE` and `README.md` files
(individual packs can use a different license, however). The path to the
`README.md` file is printed when the registry is added. It can also be
queried using the `registries::directory/2` predicate.


Registry handling
-----------------

Registries can be added using the `registries::add/2` predicate, which takes
a registry name and a registry URL. For example:

	| ?- registries::add(reg, 'https://github.com/some_user/reg.git').

Git cloning URLs are preferred but a registry can also be made available via
a local archive (using a `file://` URL) or a downloadable archive (using a
`https://` URL).

To update a registry, use the `registries::update/1-2` predicates. Registries
can also be deleted using the `registries::delete/1-2` predicate. After deleting
a registry, you can use the `packs::orphaned/0` predicate to list any orphaned
packs that are installed.

See the tool API documentation on the [registries](../../docs/registries_0.html)
object for other useful predicates.


Registry development
----------------

To simplify registry development and testing, use a local directory and a
`file://` URL when calling the `registries::add/2` predicate. If the directory
is a git repo, the tool will clone it when adding it. Otherwise, the files in
the directory are copied to the registry definition directory. This allows the
registry to be added and deleted without consequences for the registry source
files.


Pack specification
------------------

A pack is specified using a Logtalk source file defining an object that
implements the `pack_protocol`. The source file should be named after
the pack with a `_pack` suffix. The file must be available from a
declared pack registry. The pack name is ideally a valid unquoted atom.
An example of a registry specification object would be:

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
			[logtalk >= 3:36:0],
			all
		).
	
		version(
			2:0:2,
			stable,
			'https://github.com/l-flat/lflat/archive/refs/tags/v2.0.2.tar.gz',
			sha256 - '8774b3863efc03bb6c284935885dcf34f69f115656d2496a33a446b6199f3e19',
			[logtalk >= 3:36:0],
			all
		).
	
	:- end_object.

The pack sources must be available either as a local directory (when using
a `file://` URL) or for downloading as a `.zip`, `.tar.gz`, or `.tar.bz2`
archive. The checksum for the archive must use the SHA-256 hash algorithm
(`sha256`). The pack may optionally be signed.

The pack sources should contain `LICENSE` and `README.md` files. The path
to the `README.md` file is printed when the pack is installed or updated.
It can also be queried using the `packs::directory/2` predicate.


Pack versions
-------------

A pack may specify multiple versions. Each version is described using a
`version/6` predicate clause as illustrated in the example above. For
details, see the `pack_protocol` API documentation.


Pack dependencies
-----------------

Pack dependencies can be specified by pack dependency name or, for better
security, by registry and dependency names. When the pack depends on the
Logtalk version itself, the reserved name `logtalk` can be used.

- `Registry::Pack >= Version` - the pack requires a dependency with version
equal or above the specified one. For example, `logtalk >= 3:36:0` means that
the pack requires Logtalk 3.36.0 or later version.

- `Registry::Pack =< Version` - the pack requires a dependency with version
up to the specified one. For example, `common::bits =< 2:1` means that the pack
requires a `common::bits` pack up to 2.1. This includes all previous versions
and also all patches for version 2.1 (e.g. 2.1.7, 2.1.8, ...) but not version
2.2 or newer.

- `Registry::Pack < Version` - the pack requires a dependency with version
older than the specified one. For example, `common::bits < 3` means that the
pack requires a `common::bits` 2.x or older version.

- `Registry::Pack > Version` - the pack requires a dependency with version
newer than the specified one. For example, `common::bits > 2.4` means that
the pack requires a `common::bits` 2.5 or newer version.

- `Registry::Pack = Version` - the pack requires a dependency with a specific
version. For example, `common::bits = 2.1` means that the pack requires a
`common::bits` pack version 2.1.x (thus, from version 2.1.0 to the latest patch
for version 2.1).

Dependencies are specified using a list of the elements above. For example,
`[common::bits >= 2, common::bits < 3]` means all 2.x versions but not older
or newer versions.


Pack portability
----------------

Ideally, packs are fully portable and can be used with all Logtalk supported
Prolog backends. This can be declared by using the atom `all` in the last
argument of the `version/6` predicate (see example above).

When a pack can only be used with a subset of the Prolog backends, the last
argument of the `version/6` predicate is a list of backend identifiers
(atoms):

- B-Prolog: `b`
- Ciao Prolog: `ciao`
- CxProlog: `cx`
- ECLiPSe: `eclipse`
- GNU Prolog: `gnu`
- JIProlog: `ji`
- LVM: `lvm`
- Scryer Prolog: `scryer`
- SICStus Prolog: `sicstus`
- SWI-Prolog: `swi`
- Tau Prolog: `tau`
- Trealla Prolog: `trealla`
- XSB: `xsb`
- YAP: `yap`


Pack development
----------------

To simplify pack development and testing, define a local registry and add to
it a pack specification with the development version available from a local
directory. For example:

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
loading them with the `portability` flag set to `warning`.


Pack handling
-------------

Packs must be available from a defined registry. To list all packs that are
available for installation, use the `packs::available/0` predicate:

	| ?- packs::available.

To know more about a specific pack, use the `packs::describe/1-2` predicates.
For example:

	| ?- packs::describe(bar).

The `packs::describe/2` predicate can be used when two or more registries
provide packs with the same name. For example:

	| ?- packs::describe(reg, bar).

To install the latest version of a pack, we can use the `packs::install/1-4`
predicates. For example:

	| ?- packs::install(bar).

Packs becomes available for loading immediately after successful installation
(no restarting of the Logtalk session is required).

Other install predicates are available to disambiguate between registries
and to install a specific pack version. The `packs::installed/0` predicate
can be used to list all installed packs.

To update a pack, use the `packs::update/1-2` predicates. For example:

	| ?- packs::update(bar).

The tool provides versions of the pack install, update, and uninstall
predicates that accept a list of options:

- `verbose(Boolean)` (default is `false`)
- `clean(Boolean)` (default is `false`)
- `force(Boolean)` (default is `false`)
- `checksum(Boolean)` (default is `true`)
- `checksig(Boolean)` (default is `false`)

When using a `checksig(true)` option to check a pack signature, is strongly
advised that you also use the `verbose(true)` option. Note that the public key
used to sign the pack archive must be already present in your local system.

See the tool API documentation on the [packs](../../docs/packs_0.html) object
for other useful predicates.


Pinning registries and packs
----------------------------

Registries and packs can be _pinned_ after installation to prevent accidental
updating or deleting, e.g. when using the batch `update/0` predicate. This is
useful when your application requires a specific version or for security
considerations (see below). For example, if we want the `bar` pack to stay at
its current installed version:

	| ?- packs::pin(bar).
	yes

After, any attempt to update or uninstall the pack will fail with an error
message:

	| ?- packs::update(bar).
	!     Cannot update pinned pack: bar
	no

	| ?- packs::uninstall(bar).
	!     Cannot uninstall pinned pack: bar
	no

To enable the pack to be updated ou uninstalled, the pack must first be
unpinned. Alternatively, the `force(true)` option can be used. Note that
if you force update a pinned pack, the new version will be unpinned.


Testing packs
-------------

Logtalk packs (as most Logtalk libraries, tools, and examples) are expected
to have a `tester.lgt` or `tester.logtalk` tests driver file at the root of
their directory, which can be used for both automated and manual testing.
For example, after installing the `foo` pack:

	| ?- {foo(tester)}.

To test all installed packs, you can use the `logtalk_tester` automation
script from the packs directory, which you can query using the goal:

	| ?- packs::directory(Directory).

Note that running the packs tests, like simply loading the pack, can result
in calling arbitrary code, which can potentially harm your system. Always
take into account the security considerations discussed below.


Security considerations
-----------------------

New pack registries should be examined before being added, specially if
public and from a previously unknown source. The same precautions should
be taken when adding or updating a pack. Note that a registry can always
index third-party packs.

Pack checksums are checked by default. But pack signatures are only checked
if requested as packs are often unsigned. Care should be taken when adding
public keys for pack signers to your local system.

Registry and pack spec files plus the registry loader file are compiled
by term-expanding them so that only expected terms are actually loaded and
only expected `logtalk_load/2` goals are allowed. Predicates defining URLs
are discarded if the URLs are neither `https://` nor `file://` URLs or if
they contain URL search parameters. But note that this tool makes no attempt
to audit pack source files themselves.

Registries and packs can always be pinned so that they are not accidentally
updated to a version that you may not had the chance to audit.


Best practices
--------------

- Make available a new pack registry as a git repo. This simplifies updating
the registry and rolling back to a previous version.

- Use registry and pack names that are valid unquoted atoms. Use descriptive
names with underscores if necessary to link words.

- Create new pack versions from git tags.

- If the sources of a pack are available from a git repo, consider using
signed commits and signed tags for increased security.

- When a new pack version breaks backwards compatibility, list both the old
and the new versions on the pack spec file.

- Pin registries and packs when specific versions are critical for your work
so that you can still easily batch update the remaining packs and registries.

- Include the `$LOGTALKPACKS` directory (or the default `~/logtalk_packs`
directory) on your regular backups.


Installing Prolog packs
-----------------------

This tool can also be used to install Prolog packs that don't use Logtalk.
After installing a `pl_pack` Prolog pack from a `pl_reg` registry, it can
be found in the `$LOGTALKPACKS/packs/pl_reg/pl_pack` directory. When the
`LOGTALKPACKS` environment variable is not defined, the pack directory is
by default `~/logtalk_packs/packs/pl_reg/pl_pack`.

Different Prolog systems provide different solutions for locating Prolog
code. For example, several Prolog systems adopted the Quintus Prolog
`file_search_path/2` hook predicate. For these systems, a solution could
be to add a fact to this predicate for each installed Prolog pack. For
example, assuming a `pl_pack` Prolog pack:

	:- multifile(file_search_path/2).
	:- dynamic(file_search_path/2).
	
	file_search_path(library, '$LOGTALKPACKS/packs/pl_pack').

If the Prolog system also supports reading an initialization file at
startup, the above definition could be added there.
