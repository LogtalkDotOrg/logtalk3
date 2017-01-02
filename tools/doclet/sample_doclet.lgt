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


:- object(sample_doclet,
	extends(doclet)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2016/10/30,
		comment is 'Example of a doclet object.'
	]).

	% in this example doclet, we automate (re)generating both API
	% documentation and an entity diagram for the "lgtunit" tool

	% (re)generate the documentation when this file is loaded
	:- initialization(::update).

	% define one clause per goal required to generate the documentation
	% (these goals will be called in the context of "user")
	doc_goal(logtalk_load([lgtunit(loader), lgtdoc(loader), diagrams(loader)])).
	doc_goal(lgtdoc::library(lgtunit, [xml_docs_directory(docs)])).
	doc_goal(entity_diagram::library(lgtunit, [title('Logtalk lgtunit tool'), node_type_captions(true), output_directory('$LOGTALKUSER/tools/doclet/docs')])).

	% define one clause per shell command to be executed
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && lgt2html -t "API documentation for the lgtunit tool"').
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && dot -Tpdf lgtunit_entity_diagram.dot > lgtunit_entity_diagram.pdf').
	shell_command('cd "$LOGTALKUSER/tools/doclet/docs" && rm -f *.xml && rm -f *.dtd && rm -f *.xsd && rm -f custom.ent && rm -f *.dot').

:- end_object.
