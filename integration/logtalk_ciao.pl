%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration file for Ciao Prolog
%  Last updated on May 24, 2020
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- use_package(iso).

% % TODO: needed?
% :- op(600, xfy, ::).
% :- op(600,  fy, ::).
% :- op(600,  fy, ^^).
% :- op(200,  fy,  +).
% :- op(200,  fy,  ?).
% :- op(200,  fy,  @).
% :- op(200,  fy,  -).
% :- op(400, yfx, <<).
% :- op(600,  fy,  :).
% :- op(400, yfx, >>).

logtalk_ciao_init :-
	set_prolog_flag(multi_arity_warnings, off),
	current_env('LOGTALKHOME', LogtalkHome),
	% load Logtalk core files
	atom_concat(LogtalkHome, '/adapters/ciao.pl', AdapterFile), ensure_loaded(AdapterFile),
	atom_concat(LogtalkHome, '/paths/paths.pl', PathsFile), ensure_loaded(PathsFile),
	atom_concat(LogtalkHome, '/core/core.pl', CoreFile), ensure_loaded(CoreFile).

%:- initialization(logtalk_ciao_init).
?- logtalk_ciao_init.

