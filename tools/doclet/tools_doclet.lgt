%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(tools_doclet,
	extends(doclet)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/10/30,
		comment is 'Example of a doclet object generating linking diagrams using the zoom/1 option.'
	]).

	% in this example, we automate (re)generating diagrams for all the developer tools

	% (re)generate the documentation when this file is loaded
	:- initialization(::update).

	% define one clause per goal required to generate the documentation
	% (these goals will be called in the context of "user")
	doc_goal(set_logtalk_flag(source_data, on)).
	doc_goal(logtalk_load([tools(loader), ports(loader), wrapper(loader), lgtunit(tap_output), lgtunit(tap_report), lgtunit(xunit_output), lgtunit(xunit_report)])).
	doc_goal(library_dependency_diagram::rlibrary(tools, [title('Developer tools'), zoom(true)| Options])) :-
		common_options(Options).
	doc_goal(entity_diagram::library(Tool, [title(Title), zoom(true)| Options])) :-
		common_options(Options),
		tool(Tool),
		atom_concat(Tool, ' tool', Title).
	doc_goal(xref_diagram::entity(Entity, Options)) :-
		tool_entity(_, Entity),
		common_options(Options).

	% define one clause per shell command to be executed
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && cp $LOGTALKUSER/tools/diagrams/zoom.png .').
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && for f in *.dot; do dot -Tsvg $f > ${f%.*}.svg; done').
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && rm -f *.xml && rm -f *.dtd && rm -f *.xsd && rm -f custom.ent && rm -f *.dot').

	% auxiliary predicates

	common_options([
		node_type_captions(true),
		output_directory('$LOGTALKUSER/tools/doclet/docs'),
		url_prefixes('https://github.com/LogtalkDotOrg/logtalk3/tree/master/', 'http://logtalk.org/library/'),
		omit_path_prefixes(['$LOGTALKUSER/', '$LOGTALKHOME/'])
	]).

	tool(Tool) :-
		logtalk::expand_library_path(tools, ToolsDirectory),
		logtalk_library_path(Tool, _),
		logtalk::expand_library_path(Tool, ToolDirectory),
		atom_concat(ToolsDirectory, Tool, ToolDirectory0),
		atom_concat(ToolDirectory0, '/', ToolDirectory).

	tool_entity(Tool, Entity) :-
		tool(Tool),
		logtalk::expand_library_path(Tool, ToolDirectory),
		logtalk::loaded_file(File),
		logtalk::loaded_file_property(File, directory(ToolDirectory)),
		(	logtalk::loaded_file_property(File, object(Entity))
		;	logtalk::loaded_file_property(File, category(Entity))
		).

:- end_object.
