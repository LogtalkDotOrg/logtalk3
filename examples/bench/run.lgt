%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


run :-
	run(1000).

run(N) :-
	forall(
		program(Program),
		run(Program, N)
	).

run(Program, N) :-
	lgtunit::benchmark(Program::top, N, Time),
	writeq(Program), write(': '), write(Time), write(' seconds'), nl.


program(boyer).
program(chat_parser).
program(crypt).
program(derive).
program(divide10).
program(log10).
program(meta_qsort).
program(mu).
program(nreverse).
program(ops8).
program(poly_10).
program(prover).
program(qsort).
program(queens_8).
program(query).
program(reducer).
program(sendmore).
program(serialise).
program(simple_analyzer).
program(tak).
program(times10).
program(unify).
program(zebra).
:- if(predicate_property(length(_,_), built_in)).
	program(browse).
	program(fast_mu).
:- endif.
:- if(current_logtalk_flag(prolog_dialect, swi)).
	program(flatten).
:- endif.
:- if(predicate_property(recorda(_,_,_), built_in)).
	program(nand).

:- endif.
:- if(current_prolog_flag(bounded, false)).
	program(perfect).
:- endif.

