________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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


This folder contains code contributions from Logtalk users. Follows 
a short description of each included contribution:

* `flags`  
	Contributed by: Theofrastos Mantadelis

	This folder contains an implementation of persistent object
	flags. Includes usage examples.

* `iso8601`  
	Contributed by: Daniel L. Dudley

	This is a partial implementation of the ISO 8601 standard, 
	providing a library of date predicates. The time predicates 
	are not yet implemented. The best way to get acquainted with 
	this library is for you to compile the object and then run 
	one of the documentation helper scripts to transform the 
	resulting XML file into (X)HTML or PDF documentation. Your 
	feedback is appreciated.

* `pddl_parser`  
	Contributed by: Robert Sasak

	This is a partial implementation of a parser for PDDL 3.0 files.
	See `http://artax.karlin.mff.cuni.cz/~sasar5am/pddl/` for the
	original Prolog version.

* `verdi_neruda`  
	Contributed by: Victor Lagerkvist

	This folder contains a meta-interpreter collection that
	includes both top-down and bottom-up search strategies.
	See the `verdi_neruda/README.md` file for details.

* `xml_parser`  
	Contributed by: John Fletcher

	This folder contains a Logtalk version of John Fletcher's 
	Prolog XML parser (`http://www.zen37763.zen.co.uk/xml.pl.html`).
	See the `xml_parser/NOTES.md` file for details.

See the copyright and license information on the contributed files for 
usage and distributions conditions.

The `contributions/loader.lgt` file loads all contributions and is
mainly used when generating the XHTML documentation by following the
steps:

(1) Load all contributions using the query:

	| ?- {contributions(loader)}.

(2) Load the `lgtdoc` tool and generate the XML documenting files for all
library entities using the queries:

	| ?- {lgtdoc(loader)}.
	...

	| ?- lgtdoc::library(flags, [xml_docs_directory('$LOGTALKUSER/docs/tmp1')]),
		lgtdoc::library(pddl_parser, [xml_docs_directory('$LOGTALKUSER/docs/tmp2')]),
		lgtdoc::library(verdi_neruda, [xml_docs_directory('$LOGTALKUSER/docs/tmp3')]),
		lgtdoc::library(xml_parser, [xml_docs_directory('$LOGTALKUSER/docs/tmp4')]),
		lgtdoc::library(iso8601, [xml_docs_directory('$LOGTALKUSER/docs/tmp5')]).

(3) Run the command `lgt2html` on the temporary directories to generate the
(X)HTML documentation or the command `lgt2pdf` to generate PDF documentation:

	$ cd "$LOGTALKUSER/docs/tmp1" && lgt2html -i flags.html -t "Flags" && mv *.html ..
	$ cd ../tmp2 && lgt2html -i pddl_parser.html -t "PDDL Parser" && mv *.html ..
	$ cd ../tmp3 && lgt2html -i verdi_neruda.html -t "Verdi Neruda" && mv *.html ..
	$ cd ../tmp4 && lgt2html -i xml_parser.html -t "XML Parser" && mv *.html ..
	$ cd ../tmp5 && lgt2html && rm index.html && mv *.html ..

After generating the (X)HTML and/or PDF documentation, you can delete the
temporary directories:

	$ cd .. && rm -rf $LOGTALKUSER/docs/tmp*
