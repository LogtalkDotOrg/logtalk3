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

The `contributions/loader.lgt` file loads all contributions.

HTML documentation for each contribution API can be found on the `docs`
directory (open the `docs/index.html` file with your web browser). The
documentation for these tools can be regenerated using the shell scripts
`../scripts/update_html_docs.sh` and `../scripts/update_svg_diagrams.sh`.
