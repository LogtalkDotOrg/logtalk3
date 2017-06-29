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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/06/28,
		comment is 'Unit tests for the context/1 built-in method.'
	]).

	test(context_1) :-
		context(Context),
		functor(Context, logtalk, 2),
		arg(1, Context, Call), callable(Call),
		arg(2, Context, ExecutionContext), nonvar(ExecutionContext).

	test(context_2) :-
		catch({context_1_test_object::foo(a,b)}, error(Error,logtalk(Call,ExecutionContext)), true),
		Error == type_error(integer, b),
		Call == foo(a, b),
		logtalk::execution_context(ExecutionContext, Entity, Sender, This, Self, MetaCallContext, CoinductionStack),
		Entity == context_1_test_object,
		Sender == user,
		This == context_1_test_object,
		Self == context_1_test_object,
		MetaCallContext == [],
		CoinductionStack == [].		

	test(context_3) :-
		catch({logtalk::message_hook(a,b,c,d)}, error(Error,logtalk(Call,ExecutionContext)), true),
		Error == type_error(integer, d),
		Call == message_hook(a,b,c,d),
		logtalk::execution_context(ExecutionContext, Entity, Sender, This, Self, MetaCallContext, CoinductionStack),
		Entity == logtalk,
		Sender == user,
		This == logtalk,
		Self == logtalk,
		MetaCallContext == [],
		CoinductionStack == [].		

:- end_object.
