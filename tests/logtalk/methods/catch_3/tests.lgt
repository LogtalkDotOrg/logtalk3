%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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
		date is 2013/05/26,
		comment is 'Unit tests for the catch/3 built-in method.'
	]).

	test(catch_3_1) :-
		catch(true, _, _).

	test(catch_3_2) :-
		catch(a(1), _, _).

	test(catch_3_3) :-
		Goal = a(1),
		catch(Goal, _, _).

	test(catch_3_4) :-
		\+ catch(fail, _, _).

	test(catch_3_5) :-
		\+ catch(a(4), _, _).

	test(catch_3_6) :-
		Goal = a(4),
		\+ catch(Goal, _, _).

	test(catch_3_7) :-
		catch(throw(e), Error, (Error == e)).

	a(1). a(2). a(3).

:- end_object.
