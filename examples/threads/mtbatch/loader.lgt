%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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


:- initialization((
	logtalk_load(library(basic_types_loader)),
	logtalk_load(library(metapredicates_loader)),
	logtalk_load(primes(loader)),
	logtalk_load(sorting(loader)),
	logtalk_load(fibonacci(loader)),
	logtalk_load(hanoi(loader)),
	logtalk_load(tak(loader)),
	logtalk_load(fft(loader)),
	logtalk_load(integration(loader)),
	logtalk_load(integration2d(loader)),
	logtalk_load(roots(loader)),
	logtalk_load([
		searching(state_space),
		searching(heuristic_state_space),
		searching(salt3),
		searching(search_strategy),
		searching(blind_search1),
		searching(breadth_first1),
		searching(depth_first1),
		searching(heuristic_search1),
		searching(best_first1),
		searching(hill_climbing1)
	]),
	logtalk_load(mtbatch, [undefined_predicates(silent)])
)).
