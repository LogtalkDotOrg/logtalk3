%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(zoom_doclet,
	extends(doclet)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2017/02/12,
		comment is 'Example of a doclet object generating linking diagrams using the zoom/1 option.'
	]).

	% in this example doclet, we automate (re)generating both API
	% documentation and diagrams for the "dead_code_scanner" tool

	% (re)generate the documentation when this file is loaded
	:- initialization(::update).

	% define one clause per goal required to generate the documentation
	% (these goals will be called in the context of "user")
	doc_goal(set_logtalk_flag(source_data, on)).
	doc_goal(logtalk_load([dead_code_scanner(loader), lgtdoc(loader), diagrams(loader)])).
	doc_goal(lgtdoc::library(dead_code_scanner, [xml_docs_directory('$LOGTALKUSER/tools/doclet/docs'), omit_path_prefixes(['$LOGTALKUSER/', '$LOGTALKHOME/'])])).
	doc_goal(entity_diagram::library(dead_code_scanner, [title('Dead code scanner tool'), zoom(true)| Options])) :-
		common_options(Options).
	doc_goal(xref_diagram::entity(dead_code_scanner, [title('Dead code scanner main code')| Options])) :-
		common_options(Options).
	doc_goal(xref_diagram::entity(dead_code_scanner_messages, [title('Dead code scanner messages')| Options])) :-
		common_options(Options).

	% define one clause per shell command to be executed
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && lgt2html -t "API documentation for the lgtunit tool"').
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && cp $LOGTALKUSER/tools/diagrams/zoom.png .').
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && dot -Tsvg dead_code_scanner_entity_diagram.dot > dead_code_scanner_entity_diagram.svg').
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && dot -Tsvg dead_code_scanner_object_xref_diagram.dot > dead_code_scanner_object_xref_diagram.svg').
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && dot -Tsvg dead_code_scanner_messages_category_xref_diagram.dot > dead_code_scanner_messages_category_xref_diagram.svg').
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && rm -f *.xml && rm -f *.dtd && rm -f *.xsd && rm -f custom.ent && rm -f *.dot').

	% auxiliary predicates

	common_options([
		node_type_captions(true),
		output_directory('$LOGTALKUSER/tools/doclet/docs'),
		url_prefixes('https://github.com/LogtalkDotOrg/logtalk3/tree/master/', 'https://logtalk.org/library/'),
		omit_path_prefixes(['$LOGTALKUSER/', '$LOGTALKHOME/'])
	]).

:- end_object.
