%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-01-30,
		comment is 'Unit tests for the context/1 built-in method.'
	]).

	test(context_1_01, true) :-
		context(Context),
		^^assertion(functor(Context, logtalk, 2)),
		arg(1, Context, Call),
		^^assertion(callable(Call)),
		arg(2, Context, ExecutionContext),
		^^assertion(nonvar(ExecutionContext)).

	test(context_1_02, true) :-
		catch({context_1_test_object::foo(a,b)}, error(Error,logtalk(Call,ExecutionContext)), true),
		^^assertion(Error-Call == type_error(integer,b)-foo(a,b)),
		logtalk::execution_context(ExecutionContext, Entity, Sender, This, Self, MetaCallContext, CoinductionStack),
		^^assertion(c(Entity,Sender,This,Self,MetaCallContext,CoinductionStack) == c(context_1_test_object,user,context_1_test_object,context_1_test_object,[],[])).

	test(context_1_03, true) :-
		catch({context_1_test_object::bar(1)}, error(Error,logtalk(Call,ExecutionContext)), true),
		^^assertion(Error-Call == type_error(list,1)-bar(1)),
		logtalk::execution_context(ExecutionContext, Entity, Sender, This, Self, MetaCallContext, CoinductionStack),
		^^assertion(c(Entity,Sender,This,Self,MetaCallContext,CoinductionStack) == c(context_1_test_category,user,context_1_test_object,context_1_test_object,[],[])).

	test(context_1_04, true) :-
		catch({logtalk::message_hook(a,b,c,d)}, error(Error,logtalk(Call,ExecutionContext)), true),
		^^assertion(Error-Call == type_error(integer,d)-message_hook(a,b,c,d)),
		logtalk::execution_context(ExecutionContext, Entity, Sender, This, Self, MetaCallContext, CoinductionStack),
		^^assertion(c(Entity,Sender,This,Self,MetaCallContext,CoinductionStack) == c(logtalk,user,logtalk,logtalk,[],[])).

:- end_object.
