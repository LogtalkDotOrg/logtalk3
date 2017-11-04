%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(lgtdocp).

	:- info([
		version is 3.1,
		author is 'Paulo Moura',
		date is 2017/11/04,
		comment is 'Documenting tool protocol.',
		remarks is [
			'Compiling files for generating XML documentation' - 'All source files must be compiled with the "source_data" compiler flag turned on.',
			'xml_spec(Specification) option' - 'XML documenting files specification format. Possible option values are "dtd" (DTD specification; default) and "xsd" (XML Schema specification).',
			'xml_spec_reference(Reference) option' - 'Reference to the XML specification file in XML documenting files. Possible values are "local" (default; DTD/XSD file in the same folder as the XML files), "web" (logtalk.org website DTD/XSD file), and "standalone" (no reference to specification files).',
			'entity_xsl_file(File) option' - 'XSLT file to use with generated XML documenting files. Default is "logtalk_entity_to_xml.xsl", allowing the XML files to be viewed by simply opening them with a browser supporting XSLT (after running the "lgt2xml.sh" script on the XML files directory).',
			'index_xsl_file(File) option' - 'XSLT file to use with generated XML documenting files. Default is "logtalk_index_to_xml.xsl", allowing the XML files to be viewed by simply opening them with a browser supporting XSLT (after running the "lgt2xml.sh" script on the XML files directory).',
			'xml_docs_directory(Directory) option' - 'Directory where the XML documenting files will be generated. The default value is "./xml_docs", a sub-directory of the source files directory.',
			'bom(Boolean) option' - 'Defines if a BOM should be added to the generated XML documenting files.',
			'encoding(Encoding) option' - 'Encoding to be used for the generated XML documenting files.',
			'omit_path_prefixes(Prefixes) option' - 'List of path prefixes (atoms) to omit when outputting directories.',
			'exclude_files(List) option' - 'List of files to exclude when generating the XML documenting files.',
			'exclude_paths(List) option' - 'List of (relative) library paths to exclude when generating the XML documenting files.',
			'exclude_entities(List) option' - 'List of entities to exclude when generating the XML documenting files.',
			'Known issues' - 'The most appropriated options may depends on the XSL processor you intend to use. Most XSL processors support DTDs but only some of them support XML Schemas. Some processors are buggy an may not work with the default option values.'
		]
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
			'Generate XML documenting files for all tool entities for direct viewing in a browser (after indexing using the "lgt2xml" script)' - rlibrary(tools) - {yes}
		]
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
			'Generate XML documenting files for all entities in the tools directory for direct viewing in a browser (after indexing using the "lgt2xml" script)' - rdirectory('./tools') - {yes}
		]
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
		argnames is ['File']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Creates XML documenting files for all loaded entities using default options.'
	]).

	:- public(option/2).
	:- mode(option(?atom, ?nonvar), zero_or_more).
	:- info(option/2, [
		comment is 'Returns, by backtracking, all options and their values.',
		argnames is ['Option', 'Value']
	]).

	:- public(set_option/2).
	:- mode(set_option(+atom, +nonvar), zero_or_one).
	:- info(set_option/2, [
		comment is 'Sets an option value.',
		argnames is ['Option', 'Value']
	]).

:- end_protocol.
