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


:- object(doclet(_Library),
	extends(doclet)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/09/29,
		comment is 'Parametric doclet.',
		parnames is ['Library']
	]).

	% load the documenting tools
	doc_goal(logtalk_load([lgtdoc(loader), diagrams(loader)])).
	% load the application assuming a "loader.lgt" file
	doc_goal(logtalk_load(Loader)) :-
		parameter(1, Library),
		Loader =.. [Library, loader].
	% generate the library API documentation
	doc_goal(lgtdoc::rlibrary(Library, [xml_docs_directory(DocsPath)])) :-
		parameter(1, Library),
		docs_path(DocsPath).
	% generate the diagrams
	doc_goal(diagrams::rlibrary(Library, [title(Library), node_type_captions(true), output_directory(DocsPath)])) :-
		parameter(1, Library),
		docs_path(DocsPath).

	% convert the documentation to the final formats
	shell_command(Command) :-
		cd_docs(CD),
		atom_concat(CD, ' && lgt2html -t "API documentation"', Command).
	shell_command(Command) :-
		cd_docs(CD),
		atom_concat(CD, ' && for f in *.dot; do dot -Tsvg "$f" > "${f%.*}.svg"; done', Command).
	% clean up documentation intermediate files
	shell_command(Command) :-
		cd_docs(CD),
		atom_concat(CD, ' && rm -f *.xml && rm -f *.dtd && rm -f *.xsd && rm -f custom.ent && rm -f *.dot', Command).

	% auxiliary predicates

	docs_path(DocsPath) :-
		parameter(1, Library),
		logtalk::expand_library_path(Library, Path),
		atom_concat(Path, docs, DocsPath).

	cd_docs(CD) :-
		docs_path(DocsPath),
		atom_concat('cd ', DocsPath, CD).		

:- end_object.


:- object(library_doclet,
	extends(doclet(library))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/09/29,
		comment is 'Usage example of the parametric doclet to generate documentation for the standard library.'
	]).

	% (re)generate the documentation when this file is loaded
	:- initialization(::update).

:- end_object.
