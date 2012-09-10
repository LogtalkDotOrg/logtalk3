%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((
	logtalk_load(library(all_loader)),
	logtalk_load(roots(loader)),
	logtalk_load([
		searching(state_space),
		searching(water_jug),
		searching(farmer),
		searching(heuristic_state_space),
		searching(bridge),
		searching(eight_puzzle),
		searching(miss_cann),
		searching(salt3),
		searching(search_strategy),
		searching(blind_search1),
		searching(breadth_first1),
		searching(depth_first1),
		searching(heuristic_search1),
		searching(best_first1),
		searching(hill_climbing1),
		mt_hill_climbing2])
/*		loop::forto(M, 4, 8,
			(	Obj = hsalt(M, 3, 5),
				Obj::initial_state(Initial),
				write(M), write(' - '),
				{time(threaded((
						depth_first(10)::solve(Obj, Initial, Path), A = depth_first
					;	breadth_first(10)::solve(Obj, Initial, Path), A = breadth_first
					;	hill_climbing(10)::solve(Obj, Initial, Path, Cost), A = hill_climbing
%					;	mt_hill_climbing(10, 2)::solve(Obj, Initial, Path, Cost), A = mt_hill_climbing
					))
				)},
				write(A), nl, flush_output
%				Obj::print_path(Path)
			)
		)
*/	)).
