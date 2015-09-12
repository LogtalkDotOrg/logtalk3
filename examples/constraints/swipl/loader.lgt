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


:- if(current_prolog_flag(iso, true)).
	:- set_prolog_flag(iso, false).
	:- ensure_loaded(library(clpfd)).
	:- set_prolog_flag(iso, true).
:- else.
	:- ensure_loaded(library(clpfd)).
:- endif.

:- initialization((
	logtalk_load(library(basic_types_loader)),
	logtalk_load(library(metapredicates_loader)),
	logtalk_load([hexagon, queens, puzzle, sudoku, oneground])
)).

:- if((current_prolog_flag(version_data, swi(Major, Minor, _, _)), Major >= 5, Minor >= 8)).
	:- initialization(logtalk_load(knight)).
:- endif.
