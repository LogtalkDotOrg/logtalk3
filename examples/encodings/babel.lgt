:- encoding('UTF-8').		% this directive, when present, must be the first
							% term, in the first line, of a source file

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(babel).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2011/04/05,
		comment is 'Simple test of the encoding/1 directive.'
	]).

	:- public(hello_world/2).
	:- mode(hello_world(?atom, ?atom), zero_or_more).
	:- info(hello_world/2, [
		comment is 'Table of "hello world" messages in several languages (using ISO 639-2 two letter language codes for indexing).',
		argnames is ['Language', 'Text']
	]).

	hello_world(el, 'Γειά σου κόσμε!').
	hello_world(en, 'Hello world!').
	hello_world(es, '¡Hola mundo!').
	hello_world(ja, 'こんにちは世界!').
	hello_world(ko, '여보세요 세계!').
	hello_world(nl, 'Hallo wereld!').
	hello_world(pt, 'Olá mundo!').
	hello_world(ru, 'Здравствулте! мир!').
	hello_world(zh, '你好世界!').

:- end_object.
