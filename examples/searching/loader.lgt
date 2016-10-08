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


:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load(library(metapredicates_loader)),
	logtalk_load(library(dates_loader)),
	logtalk_load(roots(loader)),
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
		heuristic_search1,
		performance]),
	logtalk_load([			% the actual search methods are compiled with
		breadth_first1,		% the option events(allow) to allow the use of
		depth_first1,		% the "performance" monitor
		best_first1,
		hill_climbing1],
		[events(allow)])
)).
