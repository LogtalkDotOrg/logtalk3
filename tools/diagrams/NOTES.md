________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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


`diagrams`
==========

This tool generates *library*, *directory*, *file*, *entity*, and *predicate*
diagrams for source files and for libraries of source files using the Logtalk
reflection API to collect the relevant information and a graph language for
representing the diagrams. Limited support is also available for generating
diagrams for Prolog module applications. It's also possible in general to
generate predicate cross-referencing diagrams for plain Prolog files.

Linking *library diagrams* to *entity diagrams* to *predicate cross-referencing
diagrams* and linking *directory diagrams* to *file diagrams* is also supported
when using SVG output. This feature allows using diagrams for understanding
the architecture of applications by navigating complex code and zooming into
details. SVG output can also easily link to both source code repositories and
API documentation. This allows diagrams to be used for source code navigation.

Diagrams can also be used to uncover code issues. For example, comparing
*loading diagrams* with *dependency diagrams* can reveal implicit dependencies.
Loading diagrams can reveal circular dependencies that may warrant code
refactoring. Entity diagrams can provide a good overview of code coupling.
Predicate cross-referencing diagrams can be used to visually access entity
code complexity, complementing the `code_metrics` tool.

All diagrams support a comprehensive set of options, discussed below, to
customize the final contents and appearance.

Diagram generation can be easily automated using the `doclet` tool and the
`logtalk_doclet` scripts. See the `doclet` tool examples and documentation
for details. See also the `diagrams` tool own `lgt2svg` Bash and PowerShell
scripts.


Requirements
------------

Recent versions of d2 (0.6.9 or later) or Graphviz are required for generating
diagrams in the final formats:

- https://d2lang.com
- https://www.graphviz.org/

These can be installed using their own installers or using operating-system
package managers:

### macOS - MacPorts

	$ sudo port install d2 graphviz

### macOS - Homebrew

	$ brew install d2 graphviz

### Windows - Chocolatey

	> choco install d2 graphviz

### Installers

- https://d2lang.com/tour/install
- https://www.graphviz.org/download/


API documentation
-----------------

This tool API documentation is available at:

[../../apis/library_index.html#diagrams](../../apis/library_index.html#diagrams)

For sample queries, please see the `SCRIPT.txt` file in the tool directory.


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(diagrams(loader)).


Testing
-------

To test this tool, load the `tester.lgt` file:

	| ?- logtalk_load(diagrams(tester)).


Supported diagrams
------------------

The following entity diagrams are supported:

- *entity diagrams* showing entity public interfaces, entity inheritance
relations, and entity predicate cross-reference relations
- *predicate cross-reference diagrams* (between entities or within an entity)
- *inheritance diagrams* showing entity inheritance relations
- *uses diagrams* showing which entities use resources from other entities

The following library diagrams are supported:

- *library loading diagrams* showing which libraries load other libraries
- *library dependency diagrams* showing which libraries contain entities with
references to entities defined in other libraries

The following file diagrams are supported:

- *file loading diagrams* showing which files load or include other files
- *file dependency diagrams* showing which files contain entities with
references to entities defined in other files

File dependency diagrams are specially useful in revealing dependencies
that are not represented in file loading diagrams due to files being
loaded indirectly by files external to the libraries being documented.

The following directory diagrams are supported:

- *directory loading diagrams* showing which directories contain files that
load files from other directories
- *directory dependency diagrams* showing which directories contain entities
with references to entities defined in other directories

Comparing directory (or file) loading diagrams with directory (or file)
dependency diagrams allows comparing what is explicitly loaded with the
actual directory (or file) dependencies, which are inferred from the
source code.

Library and directory dependency diagrams are specially useful for large
applications where file diagrams would be too large and complex to be
useful, notably when combined with the *zoom* option to link to,
respectively, entity and file diagrams.

A utility object, `diagrams`, is provided for generating all supported
diagrams in one step. This object provides an interface common to all
diagrams, but note that some predicates that generate diagrams only make
sense for some types of diagrams. For best results and fine-grained
customization of each diagram, the individual diagram objects should
be used with the intended set of options.

Graph elements
--------------

Limitations in both the graph languages and UML force the invention of
a modeling language that can represent all kinds of Logtalk entities and
entity relations. Currently we use the following Graphviz node shapes
(libraries, entities, predicates, and files) and arrowheads (entity,
predicate, and file relations):

- libraries  
	`tab` (lightsalmon)
- library loading and dependency relations  
	`normal` (arrow ending with a black triangle)

- objects (classes, instances, and prototypes)  
	`box` (rectangle, yellow for instances/classes and beige for prototypes)
- protocols  
	`note` (aqua marine rectangle with folded right-upper corners)
- categories  
	`component` (light cyan rectangle with two small rectangles intercepting the left side)
- modules  
	`box` (plum rectangle with small tab at top)

- public predicates  
	`box` (springgreen)
- public, multifile, predicates  
	`box` (skyblue)
- protected predicates  
	`box` (yellow)
- private predicates  
	`box` (indianred)
- external predicates  
	`box` (beige)
- exported module predicates  
	`box` (springgreen)

- directories  
	`tab` (lightsalmon)
- directory loading and dependency relations  
	`normal` (arrow ending with a black triangle)

- files  
	`box` (pale turquoise rectangle)
- file loading and dependency relations  
	`normal` (arrow ending with a black triangle)

- specialization relation  
	`onormal` (arrow ending with a white triangle)
- instantiation relation  
	`normal` (arrow ending with a black triangle)
- extends relation  
	`vee` (arrow ending with a "v")
- implements relation  
	`dot` (arrow ending with a black circle)
- imports relation  
	`box` (arrow ending with a black square)
- complements relation  
	`obox` (arrow ending with a white square)

- uses and use module relations  
	`rdiamond` (arrow ending with a black half diamond)

- predicate calls  
	`normal` (arrow ending with a black triangle)
- dynamic predicate updates  
	`diamond` (arrow ending with a black diamond)

When using the d2 graph language, we use similar node shapes and arrowheads
when available. As d2 evolves, we hope that these graph elements will converge
further.

The library, directory, file, entity, and predicate nodes that are not part
of the predicates, entities, files, or libraries for which we are generating
a diagram use a dashed border, a darker color, and are described as external.

Note that all the elements above can have captions. See below the diagrams
`node_type_captions/1` and `relation_labels/1` output options.


Supported graph languages
-------------------------

Currently, both DOT and d2 graph languages support all the features of the
`diagrams` tool. There's also preliminary support for Mermaid (not loaded
by default, as its current version lacks several required features for parity
with d2 and Graphviz).

The diagrams `.d2` and `.dot` files are created in the current directory by
default. These files can be easily converted into a printable format such as
SVG, PDF, or Postscript. Sample helper scripts are provided for converting a
directory of `.d2` or `.dot` files to `.svg` files:

- `lgt2svg.sh` for POSIX systems
- `lgt2svg.ps1` for Windows systems

The scripts assume that the d2 and Graphviz command-line executables are
available from the system path. For Graphviz, the default is the `dot`
executable, but the scripts accept a command-line option to select in
alternative the `circo`, `fdp`, or `neato` executables). For d2, the default
layout engine is `elk`, but the scripts accept a command-line option to select
in alternative the `dagre` or `tala` layout engines.

The recommended output format is SVG, as it supports tooltips and URL links,
which can be used for showing, e.g., entity types, relation types, file paths,
and for navigating to files and directories of files (libraries) or to API
documentation. See the relevant diagram options below in order to take
advantage of these features (see the discussion below on "linking diagrams").

To convert to formats other than SVG, you will need to use the d2 and Graphviz
executables directly. For example, using the Graphviz `dot` executable, we can
generate a PDF with the command:

	dot -Tpdf diagram.dot > diagram.pdf

This usually works fine for entity and predicate call cross-referencing
diagrams. For directory and file diagrams, the `fdp` and `circo` command-line
executables may produce better results. For example:

	fdp -Tsvg diagram.dot > diagram.svg
	circo -Tsvg diagram.dot > diagram.svg

It's also worth experimenting with different layouts to find the one that
produces the best results (see the `layout/1` option described below).

When generating diagrams for multiple libraries or directories, it's possible
to split a diagram with several disconnected library or directory graphs using
the `ccomps` command-line executable. For example:

	ccomps -x -o subdiagram.dot diagram.dot

For more information on the DOT language and related tools, see:

	http://www.graphviz.org/

When using Windows, there are known issues with some Prolog compilers due
to the internal representation of paths. If you encounter problems with a
specific backend Prolog compiler, try, if possible, to use another supported
backend Prolog compiler when generating diagrams.

For printing large diagrams, you will need to either use a tool to slice
the diagram into page-sized pieces or, preferably, use software capable of
tiled printing (e.g., Adobe Reader). You can also hand-edit the generated
`.dot` files and play with settings such as aspect ratio for fine-tuning
the diagrams layout.


Customization
-------------

A set of options is available to specify the details to include in the
generated diagrams. For entity diagrams, the options are:

- `layout(Layout)`  
	diagram layout (one of the atoms `{top_to_bottom,bottom_to_top,left_to_right,right_to_left}`; default is `bottom_to_top`)

- `title(Title)`  
	diagram title (an atom; default is `''`)

- `date(Boolean)`  
	print current date and time (`true` or `false`; default is `true`)

- `versions(Boolean)`  
	print Logtalk and backend version data (`true` or `false`; default is `false`)

- `interface(Boolean)`  
	print public predicates (`true` or `false`; default is `true`)

- `file_labels(Boolean)`  
	print file labels (`true` or `false`; default is `true`)

- `file_extensions(Boolean)`  
	print file name extensions (`true` or `false`; default is `true`)

- `relation_labels(Boolean)`  
	print entity relation labels (`true` or `false`; default is `true`)

- `externals(Boolean)`  
	print external nodes (`true` or `false`; default is `true`)

- `node_type_captions(Boolean)`  
	print node type captions (`true` or `false`; default is `true`)

- `inheritance_relations(Boolean)`  
	print inheritance relations (`true` or `false`; default is `true` for entity inheritance diagrams and `false` for other entity diagrams)

- `provide_relations(Boolean)`  
	print provide relations (`true` or `false`; default is `false`)

- `xref_relations(Boolean)`  
	print predicate call cross-reference relations (`true` or `false`; default depends on the specific diagram)

- `xref_calls(Boolean)`  
	print predicate cross-reference calls (`true` or `false`; default depends on the specific diagram)

- `output_directory(Directory)`  
	directory for the `.d2` and `.dot` files (an atom; default is `'./dot_dias'`)

- `exclude_directories(Directories)`  
	list of directories to exclude except as external nodes (default is `[]`); all sub-directories of the excluded directories are also excluded; directories may be listed by full or relative path

- `exclude_files(Files)`  
	list of source files to exclude except as external nodes (default is `[]`); files may be listed by full path or basename, with or without extension

- `exclude_libraries(Libraries)`  
	list of libraries to exclude except as external nodes (default is `[startup, scratch_directory]`)

- `exclude_entities(Entities)`  
	list of entities to exclude except as external nodes (default is `[]`)

- `path_url_prefixes(PathPrefix, CodeURLPrefix, DocURLPrefix)`  
	code and documenting URL prefixes for a path prefix used when generating cluster, library, directory, file, and entity links (atoms; no default; can be specified multiple times)

- `url_prefixes(CodeURLPrefix, DocURLPrefix)`  
	default URL code and documenting URL prefixes used when generating cluster, library, file, and entity links (atoms; no default)

- `entity_url_suffix_target(Suffix, Target)`  
	extension for entity documenting URLs (an atom; default is `'.html'`) and target separating symbols (an atom; default is `'#'`)

- `omit_path_prefixes(Prefixes)`  
	omit common path prefixes when printing directory paths and when constructing URLs (a list of atoms; default is a list with the user home directory)

- `zoom(Boolean)`  
	generate sub-diagrams and add links and zoom icons to library and entity nodes (`true` or `false`; default is `false`)

- `zoom_url_suffix(Suffix)`  
	extension for linked diagrams (an atom; default is `'.svg'`)

In the particular case of cross-referencing diagrams, there are also the options:

- `recursive_relations(Boolean)`  
	print recursive predicate relations (`true` or `false`; default is `false`)

- `url_line_references(Host)`  
	syntax for the URL source file line part (an atom; possible values are `{github,gitlab,bitbucket}`; default is `github`);
	when using this option, the `CodeURLPrefix` should be a permanent link (i.e., it should include the commit SHA1)

- `predicate_url_target_format(Generator)`  
	documentation final format generator (an atom; default is `sphinx`)

For directory and file diagrams, the options are:

- `layout(Layout)`  
	diagram layout (one of the atoms `{top_to_bottom,bottom_to_top,left_to_right,right_to_left}`; default is `top_to_bottom`)

- `title(Title)`  
	diagram title (an atom; default is `''`)

- `date(Boolean)`  
	print current date and time (`true` or `false`; default is `true`)

- `versions(Boolean)`  
	print Logtalk and backend version data (`true` or `false`; default is `false`)

- `directory_paths(Boolean)`  
	print file directory paths (`true` or `false`; default is `false`)

- `file_extensions(Boolean)`  
	print file name extensions (`true` or `false`; default is `true`)

- `path_url_prefixes(PathPrefix, CodeURLPrefix, DocURLPrefix)`  
	code and documenting URL prefixes for a path prefix used when generating cluster, directory, file, and entity links (atoms; no default; can be specified multiple times)

- `url_prefixes(CodeURLPrefix, DocURLPrefix)`  
	default URL code and documenting URL prefixes used when generating cluster, library, file, and entity links (atoms; no default)

- `omit_path_prefixes(Prefixes)`  
	omit common path prefixes when printing directory paths and when constructing URLs (a list of atoms; default is a list with the user home directory)

- `relation_labels(Boolean)`  
	print entity relation labels (`true` or `false`; default is `false`)

- `externals(Boolean)`  
	print external nodes (`true` or `false`; default is `true`)

- `node_type_captions(Boolean)`  
	print node type captions (`true` or `false`; default is `false`)

- `output_directory(Directory)`  
	directory for the `.d2` and `.dot` files (an atom; default is `'./dot_dias'`)

- `exclude_directories(Directories)`  
	list of directories to exclude except as external nodes (default is `[]`)

- `exclude_files(Files)`  
	list of source files to exclude except as external nodes (default is `[]`)

- `zoom(Boolean)`  
	generate sub-diagrams and add links and zoom icons to library and entity nodes (`true` or `false`; default is `false`)

- `zoom_url_suffix(Suffix)`  
	extension for linked diagrams (an atom; default is `'.svg'`)

For library diagrams, the options are:

- `layout(Layout)`  
	diagram layout (one of the atoms `{top_to_bottom,bottom_to_top,left_to_right,right_to_left}`; default is `top_to_bottom`)

- `title(Title)`  
	diagram title (an atom; default is `''`)

- `date(Boolean)`  
	print current date and time (`true` or `false`; default is `true`)

- `versions(Boolean)`  
	print Logtalk and backend version data (`true` or `false`; default is `false`)

- `directory_paths(Boolean)`  
	print file directory paths (`true` or `false`; default is `false`)

- `path_url_prefixes(PathPrefix, CodeURLPrefix, DocURLPrefix)`  
	code and documenting URL prefixes for a path prefix used when generating cluster, library, file, and entity links (atoms; no default; can be specified multiple times)

- `url_prefixes(CodeURLPrefix, DocURLPrefix)`  
	default URL code and documenting URL prefixes used when generating cluster, library, file, and entity links (atoms; no default)

- `omit_path_prefixes(Prefixes)`  
	omit common path prefixes when printing directory paths and when constructing URLs (a list of atoms; default is a list with the user home directory)

- `relation_labels(Boolean)`  
	print entity relation labels (`true` or `false`; default is `false`)

- `externals(Boolean)`  
	print external nodes (`true` or `false`; default is `true`)

- `node_type_captions(Boolean)`  
	print node type captions (`true` or `false`; default is `false`)

- `output_directory(Directory)`  
	directory for the `.d2` and `.dot` files (an atom; default is `'./dot_dias'`)

- `exclude_directories(Directories)`  
	list of directories to exclude except as external nodes (default is `[]`)

- `exclude_files(Files)`  
	list of source files to exclude except as external nodes (default is `[]`)

- `exclude_libraries(Libraries)`  
	list of libraries to exclude except as external nodes (default is `[startup, scratch_directory]`)

- `zoom(Boolean)`  
	generate sub-diagrams and add links and zoom icons to library and entity nodes (`true` or `false`; default is `false`)

- `zoom_url_suffix(Suffix)`  
	extension for linked diagrams (an atom; default is `'.svg'`)

When using the `zoom(true)` option, the `layout(Layout)` option applies only
to the top diagram; sub-diagrams will use their own layout default.

The option `omit_path_prefixes(Prefixes)` with a non-empty list of prefixes
should preferably be used together with the option `directory_paths(true)`
when generating library or file diagrams that reference external libraries
or files. To confirm the exact default options used by each type of diagram,
send the `default_options/1` message to the diagram object.

Be sure to set the `source_data` flag `on` before compiling the libraries
or files for which you want to generate diagrams.

Support for displaying Prolog modules and Prolog module files in diagrams of
Logtalk applications:

- ECLiPSe  
	file diagrams don't display module files

- SICStus Prolog  
	file diagrams don't display module files

- SWI-Prolog  
	full support (uses the SWI-Prolog `prolog_xref` library)

- YAP  
	full support (uses the YAP `prolog_xref` library)


Linking diagrams
----------------

When using SVG output, it's possible to generate diagrams that link to other
diagrams, to API documentation, to local files and directories, and to source
code repositories.

For generating links between diagrams, use the `zoom(true)` option. This
option allows (1) linking library diagrams to entity diagrams to predicate
cross-referencing diagrams and (2) linking directory diagrams to file
diagrams to entity diagrams to predicate cross-referencing diagrams. The
sub-diagrams are automatically generated. For example, using the predicates
that generate library diagrams will also automatically generate the entity
and predicate cross-referencing diagrams.

To generate local links for opening directories, files, and file locations
in selected text editors, set the URL code prefix:

- VSCode: `url_prefixes('vscode://file/', DocPrefix)`
- VSCodium: `url_prefixes('vscodium://file/', DocPrefix)`
- Cursor: `url_prefixes('cursor://file/', DocPrefix)`
- PearAI: `url_prefixes('pearai://file/', DocPrefix)`
- Windsurf: `url_prefixes('windsurf://file/', DocPrefix)`
- Zed: `url_prefixes('zed://file/', DocPrefix)`
- BBEdit: `url_prefixes('x-bbedit://open?url=file://', DocPrefix)`
- MacVim: `url_prefixes('mvim://open?url=file://', DocPrefix)`
- TextMate: `url_prefixes('txmt://open?url=file://', DocPrefix)`
- IDEA: `url_prefixes('idea://open?file=', DocPrefix)`
- PyCharm: `url_prefixes('pycharm://open?file=', DocPrefix)`

In this case, the ``DocPrefix`` argument should be the path to the directory
containing the HTML version of the application APIs.

As most of the text editor URL scheme handlers require local links to use
absolute paths, the `omit_path_prefixes/1` option is ignored. Note that
local links require text editor support for URL schemes that can handle
both file and directory links.

To generate links to API documentation and source code repositories, use
the options `path_url_prefixes/3` (or `url_prefixes/2` for simpler cases)
and `omit_path_prefixes/1`. The idea is that the `omit_path_prefixes/1`
option specifies local file prefixes that will be cut and replaced by the
URL prefixes (which can be path prefix specific when addressing multiple
code repositories). To generate local file system URLs, define the empty
atom, `''`, as a prefix. As an example, consider the Logtalk library. Its
source code is available from a GitHub repository, and its documentation
is published on the Logtalk website. The relevant URLs in this case are:

- https://github.com/LogtalkDotOrg/logtalk3/ (source code)
- https://logtalk.org/library/ (API documentation)

Git source code URLs should include the commit SHA1 to ensure that entity
and predicate file line information in the URLs remain valid if the code
changes in later commits. Assuming a `GitHub` variable bound to the SHA1
commit URL we want to reference, an inheritance diagram can be generated
using the goal:

	| ?- GitHub  = 'https://github.com/LogtalkDotOrg/logtalk3/commit/eb156d46e135ac47ef23adcc5d20d49dd8b66abb',
	     APIDocs = 'https://logtalk.org/library/',
	     logtalk_load(diagrams(loader)),
	     set_logtalk_flag(source_data, on),
	     logtalk_load(library(all_loader)),
	     inheritance_diagram::rlibrary(library, [
	         title('Logtalk library'),
	         node_type_captions(true),
	         zoom(true),
	         path_url_prefixes('$LOGTALKUSER/', GitHub, APIDocs),
	         path_url_prefixes('$LOGTALKHOME/', GitHub, APIDocs),
	         omit_path_prefixes(['$LOGTALKUSER/', '$LOGTALKHOME/', '$HOME/'])
	     ]).

The two `path_url_prefixes/3` options take care of source code and API
documentation for entities loaded either from the Logtalk installation
directory (whose location is given by the `LOGTALKHOME` environment
variable) or from the Logtalk user directory (whose location is given
by the `LOGTALKUSER` environment variable). As we also don't want any
local operating-system paths to be exposed in the diagram, we use the
`omit_path_prefixes/1` option to suppress those path prefixes. Note
that all the paths and URLs must end with a slash for proper handling.
The `git` library may be useful to retrieve the commit SHA1 from a
local repo directory.

For both `path_url_prefixes/3` and `omit_path_prefixes/1` options, when
a path prefix is itself a prefix of another path, the shorter path must
come last to ensure correct links.

See the `SCRIPT.txt` file in the tool directory for additional examples.
To avoid retyping such complex goals when updating diagrams, use the
`doclet` tool to save and reapply them easily (e.g., by using the `make`
tool with the `documentation` target).


Creating diagrams for Prolog module applications
------------------------------------------------

Currently limited to SWI-Prolog and YAP Prolog module applications due to
the lack of a comprehensive reflection API in other Prolog systems.

Simply load your Prolog module application and its dependencies and then
use diagram entity, directory, or file predicates. Library diagram predicates
are not supported. See the `SCRIPT.txt` file in the tool directory for some
usage examples. Note that support for diagrams with links to API documentation
is quite limited, however, due to the lack of Prolog standards.


Creating diagrams for plain Prolog files
----------------------------------------

This tool can also be used to create predicate cross-referencing diagrams
for plain Prolog files. For example, if the Prolog file is named `code.pl`,
simply define an object including its code:

	:- object(code).
		:- include('code.pl').
	:- end_object.

Save the object to an e.g. `code.lgt` file in the same directory as the
Prolog file and then load it and create the diagram:

	| ?- logtalk_load(code),
	     xref_diagram::entity(code).

An alternative is to use the `object_wrapper_hook` provided by the
`hook_objects` library:

	| ?- logtalk_load(hook_objects(loader)).
	...

	| ?- logtalk_load(code, [hook(object_wrapper_hook)]),
	     xref_diagram::entity(code).


Other notes
-----------

Generating complete diagrams requires that all referenced entities are loaded.
When that is not the case, notably when generating cross-referencing diagrams,
missing entities can result in incomplete diagrams.

For complex applications, diagrams can often be made simpler and more readable
by omitting external nodes (see the `externals/1` option) and/or using one of
the alternatives to `dot` provided by Graphviz depending on the type of the
diagram (see the section above on supported graph languages for more details).

When generating entity predicate call cross-reference diagrams, caller nodes
are not created for auxiliary predicates. For example, if the `meta_compiler`
library is used to optimize meta-predicate calls, the diagrams may show
predicates that are not apparently called by any other predicate when the
callers are from the optimized meta-predicate goals (which are called via
library generated auxiliary predicates). A workaround in this case would be
creating a dedicated loader file that doesn't load (and apply) the
`meta_compiler` library when generating the diagrams.

When generating diagrams in SVG format, a copy of the `diagrams.css` file
must exist in any directory used for publishing diagrams using it. The
`lgt2svg` scripts also take care of copying this file.

The Graphviz command-line utilities, e.g., `dot`, are notorious for random
crashes (segmentation faults usually), often requiring retrying conversions
from `.dot` files to other formats. A possible workaround is to repeat the
command until it completes without error. See, for example, the `lgt2svg.sh`
script.

The conversion by the `d2` command-line executable of `.d2` files to `.svg`
files can be quite slow (as of its 0.6.8 version) with the default `elk`
layout engine. The `dagre` layout engine is much faster but doesn't support
a node referencing itself (notably, a node representing a metaclass that
instantiates itself).

Using the default d2 layout engine (`elk`) works fine with graphs with
a relatively small number of nodes and edges. When that's not the case,
it's a good idea to experiment with other layout engines.
