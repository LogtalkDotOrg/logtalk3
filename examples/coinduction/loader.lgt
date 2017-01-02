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


:- if(current_logtalk_flag(coinduction, supported)).

	:- if(current_logtalk_flag(prolog_dialect, cx)).
		:- write_depth(10, 10).
	:- endif.

	:- if(current_logtalk_flag(prolog_dialect, yap)).
		:- initialization((
			current_prolog_flag(toplevel_print_options, Options),
			set_prolog_flag(toplevel_print_options, [max_depth(10)| Options])
		)).
	:- endif.

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == eclipse; Dialect == sicstus; Dialect == swi; Dialect == yap))).
		:- initialization((
			logtalk_load(library(streamvars)),
			logtalk_load([pta, train, cotrain])
		)).
	:- endif.

	:- initialization(
		logtalk_load([
			arithmetic,
			simple,
			binary,
			streams,
			filter,
			sieve,
			lists,
			sorting,
			automata,
			counter,
			nested,
			cyclic_paths,
			shared_paths,
			tangle,
			graph
		])
	).

:- else.

	:- initialization((write('ERROR: coinduction not supported!'), nl)).

:- endif.
