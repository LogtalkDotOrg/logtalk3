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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/10/16,
		comment is 'Unit tests for the info/1 built-in directive.',
		custom is value
	]).

	:- uses(list, [
		memberchk/2
	]).

	test(info_1_01) :-
		this(This),
		object_property(This, info(Properties)),
		ground(Properties),
		memberchk(version(1.0), Properties),
		memberchk(author('Paulo Moura'), Properties),
		memberchk(date(2016/10/16), Properties),
		memberchk(comment('Unit tests for the info/1 built-in directive.'), Properties),
		memberchk(custom(value), Properties).

:- end_object.
