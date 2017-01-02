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


:- if(current_logtalk_flag(prolog_dialect, sicstus)).

	:- initialization((
		% workaround the ensure_loaded/1 built-in predicate not working
		% relative to the user visible current directory but to the directory
		% of the file being loaded (which can be different depending on the
		% value of the "scratch_directory" flag)
		current_directory(Current),
		atom_concat(Current, module, Path),
		ensure_loaded(Path),
		logtalk_load(library(os_loader)),
		logtalk_load([category], [events(deny), optimize(on)]),
		logtalk_load([objects, database, maze, graph], [events(deny), optimize(on)]),
		logtalk_load([plain, benchmarks], [events(deny), optimize(on)])
	)).

:- else.

	:- initialization((
		(current_logtalk_flag(modules, supported) -> ensure_loaded(module); true),
		logtalk_load(library(os_loader)),
		logtalk_load([category], [events(deny), optimize(on)]),
		logtalk_load([objects, database, maze, graph], [events(deny), optimize(on)]),
		logtalk_load([plain, benchmarks], [events(deny), optimize(on)])
	)).

:- endif.
