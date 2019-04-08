________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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


Overview
--------

This tool generates library, directory, file, and entity diagrams for source
files and for libraries of source files using the Logtalk reflection features
to collect the relevant information and a graph language for representing
the diagrams. Limited support is also available for generating diagrams
for Prolog module applications. It's also possible to generate predicate
cross-referencing diagrams for plain Prolog files.

Linking library and directory diagrams to entity diagrams to predicate
cross-referencing diagrams is also supported when using SVG output. This
feature allows using diagrams for navigating complex code by zooming into
details. SVG output can also easily link to both source code repositories
and API documentation. This allows diagrams to be used for source code
navigation.

All diagrams support a comprehensive set of options to customize the final
contents and appearance.

Diagram generation can be easily automated using the `doclet` tool and the
`logtalk_doclet` script. See the `doclet` tool examples and documentation
for details.


API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

[docs/library_index.html#diagrams](https://logtalk.org/docs/library_index.html#diagrams)

For sample queries, please see the [SCRIPT.txt](SCRIPT.txt) file.


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(diagrams(loader)).


Supported diagrams
------------------

The following entity diagrams are supported:

- entity diagrams showing entity public interfaces, entity inheritance
relations and entity predicate call cross-reference relations
- call diagrams showing entity predicate cross-reference calls
- inheritance diagrams showing only entity inheritance relations
- uses diagrams showing only predicate call cross-reference relations

The following file diagrams are supported:

- file loading dependency diagrams showing which files load or include
other files
- file dependency diagrams showing which files contain entities with
references to entities defined in other files

File dependency diagrams are specially useful in revealing dependencies
that are not represented in file loading diagrams due to files being
loaded indirectly by files external to the libraries being documented.

The following directory diagrams are supported:

- directory loading dependency diagrams which directories load other directories
- directory dependency diagrams showing which directories contain entities with
references to entities defined in other directories

The following library diagrams are supported:

- library loading dependency diagrams which libraries load other libraries
- library dependency diagrams showing which libraries contain entities with
references to entities defined in other libraries

Library and directory dependency diagrams are specially useful for large
applications where file diagrams would be too large and complex to be useful,
specially when combined with the *zoom* option to link to individual library
diagrams.

A utility object, `diagrams`, is provided for generating all supported
diagrams in one step. This object provides an interface common to all
diagrams but note that some predicates that generate diagrams only make
sense for some types of diagrams.

Limitations in both the graph language and UML forces the invention of a
modeling language that can represent all kinds of Logtalk entities and
entity relations. Currently we use the following DOT shapes (libraries,
entities, predicates, and files) and arrows (entity, predicate, and file
relations):

- libraries  
	`box3d` (3D box, lightyellow)
- library loading and dependency relations  
	`normal` (arrow ending with a black triangle)

- objects (classes, instances, and prototypes)  
	`box` (rectangle, yellow for instances/classes and beige for prototypes)
- protocols  
	`note` (aqua marine rectangle with folded right-upper corners)
- categories  
	`component` (light cyan rectangle with two small rectangles intercepting the left side)
- modules  
	`tab` (grey rectangle with small tab at top)

- public predicates  
	`ellipse` (green)
- public, multifile, predicates  
	`ellipse` (blue)
- protected predicates  
	`ellipse` (yellow)
- private predicates  
	`ellipse` (red)
- predicates not locally declared  
	`ellipse` (beige)
- exported module predicates  
	`ellipse` (green)

- directories  
	`box3d` (3D box, lightyellow)
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

The library, directory, file, entity, and predicate nodes that are not part
of the predicates, entities, files, or libraries for which we are generating
a diagram use a dashed border.


Supported graph languages
-------------------------

Currently only the DOT graph language is supported (tested with Graphviz
version 2.38.0; visit the <http://www.graphviz.org/> website for more
information). Versions 2.40 and 2.41 have a nasty regression in the SVG
exporter where text overflows the boxes that should contain it.

The diagrams `.dot` files are created on the current directory by default.
These files can be easily converted into a printable format such as SVG, PDF,
or Postscript. For example, using the `dot` command-line executable we can
simply type:

	dot -Tpdf diagram.dot > diagram.pdf

This usually works fine for entity and predicate call cross-referencing
diagrams. For file diagrams, the `circo` command-line executable often
produces better results:

	circo -Tpdf diagram.dot > diagram.pdf

It's also worth to experiment with different layouts to find the one that
produces the best results (see the `layout/1` option described below).

Some output formats such as SVG support tooltips and URL links, which can
be used for showing e.g. entity types, relation types, file paths, and for
navigating to files and directories of files (libraries). See the relevant
diagram options below in order to take advantage of these features.

When generating diagrams for multiple libraries or directories, it's possible
to split a diagram with several disconnected library or directory graphs using
the `ccomps` command-line executable. For example:

	ccomps -x -o subdiagram.dot diagram.dot

For more information on the DOT language and related tools see:

	http://www.graphviz.org/

When using Windows, there are known issues with some Prolog compilers due
to the internal representation of paths. If you encounter problems with a
specific backend Prolog compiler, try to use another supported backend
Prolog compiler when generating diagrams.

For printing large diagrams, you will need to either use a tool to slice
the diagram in page-sized pieces or, preferably, use software capable of
tiled printing (e.g. Adobe Reader). You can also hand-edit the generated
`.dot` files and play with settings such as aspect ratio for fine-tuning
the diagrams layout.


Customization
-------------

A set of options are available to specify the details to include in the
generated diagrams. For entity diagrams the options are:

- `layout(Layout)`  
	diagram layout (one of the atoms `{top_to_bottom,bottom_to_top,left_to_right,right_to_left}`; default is `bottom_to_top`)
- `title(Title)`  
	diagram title (an atom; default is `''`)
- `date(Boolean)`  
	print/omit current date and time (`true` or `false`; default is `true`)
- `interface(Boolean)`  
	print/omit public predicates (`true` or `false`; default is `true`)
- `file_labels(Boolean)`  
	print/omit file labels (`true` or `false`; default is `true`)
- `file_extensions(Boolean)`  
	print/omit file name extensions (`true` or `false`; default is `true`)
- `relation_labels(Boolean)`  
	print/omit entity relation labels (`true` or `false`; default is `true`)
- `node_type_captions(Boolean)`  
	print/omit node type captions (`true` or `false`; default is `true`)
- `inheritance_relations(Boolean)`  
	print/omit inheritance relations (`true` or `false`; default is `true` for entity inheritance diagrams and `false` for other entity diagrams)
- `provide_relations(Boolean)`  
	print/omit provide relations (`true` or `false`; default is `false`)
- `xref_relations(Boolean)`  
	print/omit predicate call cross-reference relations (`true` or `false`; default depends on the specific diagram)
- `xref_calls(Boolean)`  
	print/omit predicate cross-reference calls (`true` or `false`; default depends on the specific diagram)
- `output_directory(Directory)`  
	directory for the .dot files (an atom; default is `'./'`)
- `exclude_directories(Directories)`  
	list of directories to exclude (default is `[]`)
- `exclude_files(Files)`  
	list of source files to exclude (default is `[]`)
- `exclude_libraries(Libraries)`  
	list of libraries to exclude (default is `[startup, scratch_directory]`)
- `exclude_entities(Entities)`  
	list of entities to exclude (default is `[]`)
- `path_url_prefixes(PathPrefix, CodeURLPrefix, DocURLPrefix)`  
	code and documenting URL prefixes for a path prefix used when generating cluster, library, directory, file, and entity links (atoms; no default)
- `url_prefixes(CodeURLPrefix, DocURLPrefix)`  
	default code and documenting URL prefixes used when generating cluster, library, directory, file, and entity links (atoms; defaults are `''`)
- `entity_url_suffix_target(Suffix, Target)`  
	extension for entity documenting URLs (an atom; default is `'.html'`) and target separating symbols (an atom; default is `'#'`)
- `omit_path_prefixes(Prefixes)`  
	omit common path prefixes when printing directory paths (a list of atoms; default is `[]`)
- `zoom(Boolean)`  
	generate sub-diagrams and add zoom links and icons to library and entity nodes (`true` or `false`; default is `false`)
- `zoom_url_suffix(Suffix)`  
	extension for zoom linked diagrams (an atom; default is `'.svg'`)

In the particular case of cross-referencing diagrams, there is also the option:

- `url_line_references(Host)`  
	syntax for the URL source file line part (an atom; possible values are `{github,gitlab,bitbucket}`; default is `github`);
	when using this option, the `CodeURLPrefix` should be a permanent link (i.e. it should include the commit SHA1)

For directory and file diagrams the options are:

- `layout(Layout)`  
	diagram layout (one of the atoms `{top_to_bottom,bottom_to_top,left_to_right,right_to_left}`; default is `top_to_bottom`)
- `title(Title)`  
	diagram title (an atom; default is `''`)
- `date(Boolean)`  
	print/omit current date and time (`true` or `false`; default is `true`)
- `directory_paths(Boolean)`  
	print/omit file directory paths (`true` or `false`; default is `false`)
- `file_extensions(Boolean)`  
	print/omit file name extensions (`true` or `false`; default is `true`)
- `path_url_prefixes(PathPrefix, CodeURLPrefix, DocURLPrefix)`  
	code and documenting URL prefixes for a path prefix used when generating cluster, directory, file, and entity links (atoms; no default)
- `url_prefixes(CodeURLPrefix, DocURLPrefix)`  
	default URL code and documenting URL prefixes used when generating cluster, directory, file, and entity links (atoms; defaults are `''`)
- `omit_path_prefixes(Prefixes)`  
	omit common path prefixes when printing directory paths (a list of atoms; default is `[]`)
- `relation_labels(Boolean)`  
	print/omit entity relation labels (`true` or `false`; default is `false`)
- `node_type_captions(Boolean)`  
	print/omit node type captions (`true` or `false`; default is `false`)
- `output_directory(Directory)`  
	directory for the .dot files (an atom; default is `'./'`)
- `exclude_directories(Directories)`  
	list of directories to exclude (default is `[]`)
- `exclude_files(Files)`  
	list of source files to exclude (default is `[]`)
- `exclude_libraries(Libraries)`  
	list of libraries to exclude (default is `[startup, scratch_directory]`)
- `zoom(Boolean)`  
	generate sub-diagrams and add zoom links and icons to library and entity nodes (`true` or `false`; default is `false`)
- `zoom_url_suffix(Suffix)`  
	extension for zoom linked diagrams (an atom; default is `'.svg'`)

For library diagrams the options are:

- `layout(Layout)`  
	diagram layout (one of the atoms `{top_to_bottom,bottom_to_top,left_to_right,right_to_left}`; default is `top_to_bottom`)
- `title(Title)`  
	diagram title (an atom; default is `''`)
- `date(Boolean)`  
	print/omit current date and time (`true` or `false`; default is `true`)
- `directory_paths(Boolean)`  
	print/omit file directory paths (`true` or `false`; default is `false`)
- `path_url_prefixes(PathPrefix, CodeURLPrefix, DocURLPrefix)`  
	code and documenting URL prefixes for a path prefix used when generating cluster, library, file, and entity links (atoms; no default)
- `url_prefixes(CodeURLPrefix, DocURLPrefix)`  
	default URL code and documenting URL prefixes used when generating cluster, library, file, and entity links (atoms; defaults are `''`)
- `omit_path_prefixes(Prefixes)`  
	omit common path prefixes when printing directory paths (a list of atoms; default is `[]`)
- `relation_labels(Boolean)`  
	print/omit entity relation labels (`true` or `false`; default is `false`)
- `node_type_captions(Boolean)`  
	print/omit node type captions (`true` or `false`; default is `false`)
- `output_directory(Directory)`  
	directory for the .dot files (an atom; default is `'./'`)
- `exclude_directories(Directories)`  
	list of directories to exclude (default is `[]`)
- `exclude_files(Files)`  
	list of source files to exclude (default is `[]`)
- `exclude_libraries(Libraries)`  
	list of libraries to exclude (default is `[startup, scratch_directory]`)
- `zoom(Boolean)`  
	generate sub-diagrams and add zoom links and icons to library and entity nodes (`true` or `false`; default is `false`)
- `zoom_url_suffix(Suffix)`  
	extension for zoom linked diagrams (an atom; default is `'.svg'`)

The option `omit_path_prefixes(Prefixes)` with a non-empty list of prefixes
should be used together with the option `directory_paths(true)`, in particular
when generating diagrams for libraries and directories with external files.

Be sure to set the `source_data` flag `on` before compiling the libraries
or files for which you want to generated diagrams.

Support for displaying Prolog modules and Prolog module files in diagrams:

- ECLiPSe  
	file diagrams don't display module files
- SICStus Prolog  
	file diagrams don't display module files
- SWI-Prolog  
	full support (uses the SWI-Prolog `prolog_xref` library)
- YAP  
	full support (uses the YAP `prolog_xref` library)

When using SWI-Prolog or YAP as the backend compilers, diagrams can also be
generated for (loaded) Prolog source files (containing module definitions)
and for (loaded) Prolog modules. However, the diagraming methods that take
a library name as argument are not currently usable.


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

	?- logtalk_load(code), xref_diagram::entity(code).


Other notes
-----------

Generating complete diagrams requires that all referenced entities are loaded.
When that is not the case, notably when generating cross-referencing diagrams,
missing entities can result in incomplete diagrams.

The zoom icons, `zoom.png` and `zoom.svg` have been designed by Xinh Studio:

	https://www.iconfinder.com/xinhstudio

Currently, only the `zoom.png` file is used. A copy of this file must exist
in any directory used for publishing diagrams using it.

The Graphviz command-line utilities, e.g. `dot`, are notorious for random
crashes (segmentation faults usually), often requiring re-doing conversions
from `.dot` files to other formats. A possible workaround is to repeat the
command until it completes without error. For example:

	$ cp "$LOGTALKUSER/tools/diagrams/zoom.png" . && for file in *.dot; do flag=0; counter=10; while [ $flag -eq 0 ] && [ $counter -ge 0 ] ; do dot -Tsvg $file > ${file%.*}.svg; if [ $? == 0 ]; then flag=1; fi; (( --counter )); done; done

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
