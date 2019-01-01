%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load(library(metapredicates_loader)),
	logtalk_load(library(dates_loader)),
	logtalk_load(roots(loader)),
	set_logtalk_flag(events, allow),
	% puzzles
	logtalk_load([
		state_space,
		water_jug,
		farmer,
		heuristic_state_space,
		bridge,
		eight_puzzle,
		miss_cann,
		salt3,
		search_strategy,
		blind_search1,
		heuristic_search1
	]),
	% search methods
	logtalk_load([
		breadth_first1,
		depth_first1,
		best_first1,
		hill_climbing1
	]),
	% monitors
	logtalk_load([
		performance
	])
)).
