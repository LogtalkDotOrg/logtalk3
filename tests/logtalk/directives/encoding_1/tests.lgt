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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/27,
		comment is 'Unit tests for the encoding/1 built-in directive.'
	]).

	test(encoding_1_1) :-
		logtalk::loaded_file_property(File, basename('iso_8859_1.lgt')),
		logtalk::loaded_file_property(File, text_properties(Properties)),
		member(encoding('ISO-8859-1'), Properties),
		\+ member(encoding('UTF-8'), Properties).

	test(encoding_1_2) :-
		logtalk::loaded_file_property(File, basename('utf_8_bom.lgt')),
		logtalk::loaded_file_property(File, text_properties(Properties)),
		member(encoding('UTF-8'), Properties),
		member(bom(true), Properties).

	test(encoding_1_3) :-
		logtalk::loaded_file_property(File, basename('utf_8_no_bom.lgt')),
		logtalk::loaded_file_property(File, text_properties(Properties)),
		member(encoding('UTF-8'), Properties),
		\+ member(bom(true), Properties).

	member(Element, [Head| _]) :-
		Element == Head,
		!.
	member(Element, [_| Tail]) :-
		member(Element, Tail).

:- end_object.
