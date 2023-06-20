%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% linter flags

:- initialization((
	create_logtalk_flag(lgtdoc_non_standard_exceptions, warning, [type(atom), keep(true)]),
	create_logtalk_flag(lgtdoc_missing_directives,      warning, [type(atom), keep(true)]),
	create_logtalk_flag(lgtdoc_missing_info_key,        warning, [type(atom), keep(true)]),
	create_logtalk_flag(lgtdoc_missing_punctuation,     warning, [type(atom), keep(true)]),
	create_logtalk_flag(lgtdoc_invalid_dates,           warning, [type(atom), keep(true)])
)).


:- protocol(lgtdocp).

	:- info([
		version is 5:1:0,
		author is 'Paulo Moura',
		date is 2023-06-20,
		comment is 'Documenting tool protocol.',
		remarks is [
			'Compiling files for generating XML documentation' - 'All source files must be compiled with the ``source_data`` flag turned on.',
			'xml_spec(Specification) option' - 'XML documenting files specification format. Possible option values are ``dtd`` (DTD specification; default) and ``xsd`` (XML Schema specification).',
			'xml_spec_reference(Reference) option' - 'Reference to the XML specification file in XML documenting files. Possible values are ``local`` (default; DTD/XSD file in same folder as XML files), ``web`` (logtalk.org website DTD/XSD file), and ``standalone`` (no reference to specification files).',
			'entity_xsl_file(File) option' - 'XSLT file to use with generated XML documenting files. Default is ``logtalk_entity_to_xml.xsl``, allowing the XML files to be viewed by opening them with a browser supporting XSLT (after running the ``lgt2xml.sh`` script on the output directory).',
			'index_xsl_file(File) option' - 'XSLT file to use with generated XML documenting files. Default is ``logtalk_index_to_xml.xsl``, allowing the XML files to be viewed by opening them with a browser supporting XSLT (after running the ``lgt2xml.sh`` script on the output directory).',
			'xml_docs_directory(Directory) option' - 'Directory where the XML documenting files will be generated. The default value is ``./xml_docs``, a sub-directory of the source files directory.',
			'bom(Boolean) option' - 'Defines if a BOM should be added to the generated XML documenting files.',
			'encoding(Encoding) option' - 'Encoding to be used for the generated XML documenting files.',
			'omit_path_prefixes(Prefixes) option' - 'List of path prefixes (atoms) to omit when writing directory paths. The default value is to omit the home directory.',
			'exclude_files(List) option' - 'List of files to exclude when generating the XML documenting files.',
			'exclude_paths(List) option' - 'List of relative library paths to exclude when generating the XML documenting files (default is ``[]``).',
			'exclude_prefixes(List) option' - 'List of path prefixes to exclude when generating the XML documenting files (default is ``[]``).',
			'exclude_entities(List) option' - 'List of entities to exclude when generating the XML documenting files (default is ``[]``).',
			'sort_predicates(Boolean) option' - 'Sort entity predicates (default is ``false``).',
			'Known issues' - 'The most appropriated options may depends on the XSL processor you intend to use. Most XSL processors support DTDs but only some of them support XML Schemas. Some processors are buggy an may not work with the default option values.'
		],
		see_also is [lgtdoc]
	]).

	:- public(rlibraries/2).
	:- mode(rlibraries(+list(atom), +list), one).
	:- info(rlibraries/2, [
		comment is 'Creates XML documenting files for all entities in all given libraries and their sub-libraries using the specified options.',
		argnames is ['Libraries', 'Options']
	]).

	:- public(rlibraries/1).
	:- mode(rlibraries(+list(atom)), one).
	:- info(rlibraries/1, [
		comment is 'Creates XML documenting files for all entities in all given libraries and their sub-libraries using default options.',
		argnames is ['Libraries']
	]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list), one).
	:- info(rlibrary/2, [
		comment is 'Creates XML documenting files for all entities in a library and its sub-libraries using the specified options.',
		argnames is ['Library', 'Options'],
		examples is [
			'Generate XML documenting files for all tool entities for later conversion to Markdown files' - rlibrary(tools,[xslfile('lgtmd.xsl')]) - {yes}
		]
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Creates XML documenting files for all entities in a library and its sub-libraries using default options.',
		argnames is ['Library'],
		examples is [
			'Generate XML documenting files for all tool entities for direct viewing in a browser (after indexing using the ``lgt2xml`` script)' - rlibrary(tools) - {yes}
		]
	]).

	:- public(libraries/2).
	:- mode(libraries(+list(atom), +list), one).
	:- info(libraries/2, [
		comment is 'Creates XML documenting files for all entities in all given libraries using the specified options.',
		argnames is ['Libraries', 'Options']
	]).

	:- public(libraries/1).
	:- mode(libraries(+list(atom)), one).
	:- info(libraries/1, [
		comment is 'Creates XML documenting files for all entities in all given libraries using default options.',
		argnames is ['Libraries']
	]).

	:- public(library/2).
	:- mode(library(+atom, +list), one).
	:- info(library/2, [
		comment is 'Creates XML documenting files for all entities in a library using the specified options.',
		argnames is ['Library', 'Options'],
		examples is [
			'Generate XML documenting files for all library entities for later conversion to PDF A4 files' - library(library,[xslfile('logtalk_entity_to_pdf_a4.xsl')]) - {yes}
		]
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates XML documenting files for all entities in a library using default options.',
		argnames is ['Library']
	]).

	:- public(rdirectories/2).
	:- mode(rdirectories(+list(atom), +list), one).
	:- info(rdirectories/2, [
		comment is 'Creates XML documenting files for all entities in all given directories and their sub-directories using the specified options.',
		argnames is ['Directories', 'Options']
	]).

	:- public(rdirectories/1).
	:- mode(rdirectories(+list(atom)), one).
	:- info(rdirectories/1, [
		comment is 'Creates XML documenting files for all entities in all given directories and their sub-directories using default options.',
		argnames is ['Directories']
	]).

	:- public(rdirectory/2).
	:- mode(rdirectory(+atom, +list), one).
	:- info(rdirectory/2, [
		comment is 'Creates XML documenting files for all entities in a directory and its sub-directories using the specified options.',
		argnames is ['Directory', 'Options'],
		examples is [
			'Generate XML documenting files for all entities in the tools directory for later conversion to Markdown files' - rdirectory('./tools',[xslfile('lgtmd.xsl')]) - {yes}
		]
	]).

	:- public(rdirectory/1).
	:- mode(rdirectory(+atom), one).
	:- info(rdirectory/1, [
		comment is 'Creates XML documenting files for all entities in a directory and its sub-directories using default options.',
		argnames is ['Directory'],
		examples is [
			'Generate XML documenting files for all entities in the tools directory for direct viewing in a browser (after indexing using the ``lgt2xml`` script)' - rdirectory('./tools') - {yes}
		]
	]).

	:- public(directories/2).
	:- mode(directories(+list(atom), +list), one).
	:- info(directories/2, [
		comment is 'Creates XML documenting files for all entities in all given directories using the specified options.',
		argnames is ['Directories', 'Options']
	]).

	:- public(directories/1).
	:- mode(directories(+list(atom)), one).
	:- info(directories/1, [
		comment is 'Creates XML documenting files for all entities in all given directories using default options.',
		argnames is ['Directories']
	]).

	:- public(directory/2).
	:- mode(directory(+atom, +list), one).
	:- info(directory/2, [
		comment is 'Creates XML documenting files for all entities in a directory using the specified options.',
		argnames is ['Directory', 'Options'],
		examples is [
			'Generate XML documenting files for all the entities in the current directory for later conversion to PDF A4 files' - directory('.',[xslfile('logtalk_entity_to_pdf_a4.xsl')]) - {yes}
		]
	]).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Creates XML documenting files for all entities in a directory using default options.',
		argnames is ['Directory']
	]).

	:- public(files/2).
	:- mode(files(+list(atom), +list), one).
	:- info(files/2, [
		comment is 'Creates XML documenting files for all entities in loaded source files using the specified options. The files can be given by name, basename, full path, or using library notation.',
		argnames is ['Files', 'Options']
	]).

	:- public(files/1).
	:- mode(files(+list(atom)), one).
	:- info(files/1, [
		comment is 'Creates XML documenting files for all entities in loaded source files using default options. The files can be given by name, basename, full path, or using library notation.',
		argnames is ['Files']
	]).

	:- public(file/2).
	:- mode(file(+atom, +list), one).
	:- info(file/2, [
		comment is 'Creates XML documenting files for all entities in a loaded source file using the specified options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['File', 'Options']
	]).

	:- public(file/1).
	:- mode(file(+atom), one).
	:- info(file/1, [
		comment is 'Creates XML documenting files for all entities in a loaded source file using default options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['File']
	]).

	:- public(all/1).
	:- mode(all(+list), one).
	:- info(all/1, [
		comment is 'Creates XML documenting files for all loaded entities using the specified options.',
		argnames is ['Options']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Creates XML documenting files for all loaded entities using default options.'
	]).

:- end_protocol.
