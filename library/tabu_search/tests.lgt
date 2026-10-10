%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-10,
		comment is 'Unit tests for the "tabu_search" library.'
	]).

	:- uses(list, [
		msort/2, length/2, memberchk/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	cover(tabu_search(_, _)).
	cover(tabu_search(_)).

	test(tabu_search_private_test_access, deterministic) :-
		tabu_search(quadratic, xoshiro128pp)<<sample_list([a], 1, 1, [a]).

	% quadratic problem - modest step budgets keep CI fast while still
	% exercising the core search loop and energy improvement

	test(ts_quadratic_run_2, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [max_steps(500)]).

	test(ts_quadratic_run_3_default, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [max_steps(500)]).

	test(ts_quadratic_run_3_more_steps, deterministic((number(Energy), Energy < 1.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [max_steps(2000), candidates(30)]).

	test(ts_quadratic_state_is_number, deterministic(number(State))) :-
		tabu_search(quadratic)::run(State, _Energy, [max_steps(200)]).

	test(ts_quadratic_energy_non_negative, deterministic(Energy >= 0.0)) :-
		tabu_search(quadratic)::run(_State, Energy, [max_steps(200)]).

	% TSP problem

	test(ts_tsp_run_2, deterministic((list::valid(State), number(Energy)))) :-
		tabu_search(tsp)::run(State, Energy, [max_steps(300), candidates(15)]).

	test(ts_tsp_tour_is_permutation, deterministic(Sorted == Expected)) :-
		tabu_search(tsp)::run(Tour, _Energy, [max_steps(200), candidates(10)]),
		msort(Tour, Sorted),
		msort([a, b, c, d, e, f], Expected).

	test(ts_tsp_tour_has_six_cities, deterministic(Length == 6)) :-
		tabu_search(tsp)::run(Tour, _Energy, [max_steps(100), candidates(8)]),
		length(Tour, Length).

	test(ts_tsp_energy_below_naive, deterministic(Energy < 50.0)) :-
		tabu_search(tsp)::run(_Tour, Energy, [max_steps(800), candidates(20)]).

	% option validation (no search work)

	test(ts_invalid_option_max_steps, error(domain_error(option, max_steps(-1)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [max_steps(-1)]).

	test(ts_invalid_option_tabu_tenure, error(domain_error(option, tabu_tenure(-1)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [tabu_tenure(-1)]).

	test(ts_invalid_option_candidates, error(domain_error(option, candidates(0)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [candidates(0)]).

	test(ts_invalid_option_seed, error(domain_error(option, seed(-1)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [seed(-1)]).

	% run/4 returns statistics

	test(ts_run_4_returns_statistics, deterministic) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [max_steps(200)]),
		memberchk(steps(Steps), Statistics),
		assertion((integer(Steps), Steps > 0)),
		memberchk(acceptances(Acc), Statistics),
		assertion((integer(Acc), Acc >= 0)),
		memberchk(improvements(Imp), Statistics),
		assertion((integer(Imp), Imp >= 0)),
		memberchk(final_tabu_size(Size), Statistics),
		assertion((integer(Size), Size >= 0)).

	test(ts_run_4_steps_match_max, deterministic(Steps =:= 300)) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [max_steps(300)]),
		memberchk(steps(Steps), Statistics).

	test(ts_run_4_acceptances_bounded, deterministic((Acc >= 0, Acc =< Steps))) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [max_steps(200)]),
		memberchk(steps(Steps), Statistics),
		memberchk(acceptances(Acc), Statistics).

	% tabu tenure

	test(ts_tabu_tenure_respected, deterministic(Size =< 5)) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [tabu_tenure(5), max_steps(50)]),
		memberchk(final_tabu_size(Size), Statistics).

	test(ts_tabu_tenure_range_runs, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [tabu_tenure_range(3, 9), max_steps(400)]).

	test(ts_tabu_tenure_range_overrides_fixed, deterministic((number(Energy), Energy < 5.0))) :-
		% range present -> fixed tenure is ignored
		tabu_search(quadratic)::run(_State, Energy, [tabu_tenure(2), tabu_tenure_range(4, 8), max_steps(300)]).

	test(ts_tabu_tenure_range_statistics, deterministic) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [tabu_tenure_range(2, 6), max_steps(100)]),
		memberchk(steps(Steps), Statistics),
		assertion(Steps =:= 100),
		memberchk(final_tabu_size(Size), Statistics),
		assertion((integer(Size), Size >= 0)).

	test(ts_tabu_tenure_range_seed_reproducible, deterministic(E1 =:= E2)) :-
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_S1, E1, [seed(55), tabu_tenure_range(3, 7), max_steps(200)]),
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_S2, E2, [seed(55), tabu_tenure_range(3, 7), max_steps(200)]).

	test(ts_invalid_option_tabu_tenure_range_min, error(domain_error(option, tabu_tenure_range(0, 5)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [tabu_tenure_range(0, 5)]).

	test(ts_invalid_option_tabu_tenure_range_order, error(domain_error(option, tabu_tenure_range(9, 3)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [tabu_tenure_range(9, 3)]).

	% seed option for reproducibility

	test(ts_seed_reproducible_results, deterministic(Energy1 =:= Energy2)) :-
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_State1, Energy1, [seed(42), max_steps(300)]),
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_State2, Energy2, [seed(42), max_steps(300)]).

	test(ts_seed_reproducible_state, deterministic(State1 =:= State2)) :-
		quadratic::reset_seed,
		tabu_search(quadratic)::run(State1, _Energy1, [seed(42), max_steps(300)]),
		quadratic::reset_seed,
		tabu_search(quadratic)::run(State2, _Energy2, [seed(42), max_steps(300)]).

	% neighbor_state/3 delta-energy variant

	test(ts_delta_energy_run_2, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic_delta)::run(_State, Energy, [max_steps(500)]).

	test(ts_delta_energy_run_4, deterministic((integer(Steps), Steps > 0))) :-
		tabu_search(quadratic_delta)::run(_State, _Energy, Statistics, [max_steps(300)]),
		memberchk(steps(Steps), Statistics).

	% progress reporting

	test(ts_progress_updates_called, deterministic(Count > 0)) :-
		quadratic_progress::clear_log,
		tabu_search(quadratic_progress)::run(_State, _Energy, [updates(3), max_steps(150)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	test(ts_progress_updates_zero, deterministic(Count =:= 0)) :-
		quadratic_progress::clear_log,
		tabu_search(quadratic_progress)::run(_State, _Energy, [updates(0), max_steps(50)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	% restarts

	test(ts_restarts_zero_default, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [restarts(0), max_steps(400)]).

	test(ts_restarts_steps_accumulate, deterministic(Steps > 450)) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [restarts(2), max_steps(200)]),
		memberchk(steps(Steps), Statistics).

	test(ts_restarts_seed_reproducible, deterministic(E1 =:= E2)) :-
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_S1, E1, [seed(77), restarts(1), max_steps(200)]),
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_S2, E2, [seed(77), restarts(1), max_steps(200)]).

	test(ts_invalid_option_restarts, error(domain_error(option, restarts(-1)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [restarts(-1)]).

	test(tabu_search_large_full_energy, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-2.0e300, b-1.5e300], none))::run(State, Energy, Statistics, [max_steps(1)]),
		assertion(State == b),
		assertion(lgtunit::(Energy =~= 1.5e300)),
		assertion(Statistics == [steps(1), acceptances(1), improvements(1), final_tabu_size(1)]).

	test(tabu_search_large_sampled_energy, deterministic) :-
		tabu_search(tabu_search_fixture(sampled, [a-2.0e300, b-1.5e300], none))::run(State, Energy, Statistics, [max_steps(1), candidates(2)]),
		assertion(State == b),
		assertion(lgtunit::(Energy =~= 1.5e300)),
		assertion(Statistics == [steps(1), acceptances(1), improvements(1), final_tabu_size(1)]).

	test(tabu_search_sentinel_boundary, deterministic) :-
		forall(list::member(Mode, [full, sampled]), (
			tabu_search(tabu_search_fixture(Mode, [a-2.0e300, b-1.0e300], none))::run(State, Energy, [max_steps(1)]),
			assertion(State == b),
			assertion(lgtunit::(Energy =~= 1.0e300))
		)).

	test(tabu_search_large_worsening_move, deterministic) :-
		forall(list::member(Mode, [full, sampled]), (
			tabu_search(tabu_search_fixture(Mode, [a-1.5e300, b-2.0e300], none))::run(State, Energy, Statistics, [max_steps(1)]),
			assertion(State == a),
			assertion(lgtunit::(Energy =~= 1.5e300)),
			assertion(Statistics == [steps(1), acceptances(1), improvements(0), final_tabu_size(1)])
		)).

	test(tabu_search_empty_neighborhood, deterministic) :-
		tabu_search(tabu_search_fixture(empty, [a-0], none))::run(a, 0, Statistics, [max_steps(2)]),
		assertion(Statistics == [steps(2), acceptances(0), improvements(0), final_tabu_size(0)]).

	test(tabu_search_candidate_tie, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-2, b-1, c-1], none), xoshiro128pp)<<evaluate_candidates(policy(false, false, false, false, false), [c,b], 2, [], 0, State, Energy, Accepted),
		assertion(State == c),
		assertion(Energy == 1),
		assertion(Accepted == true).

	test(tabu_search_all_candidates_tabu, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0, b-1], none), xoshiro128pp)<<evaluate_candidates(policy(false, false, false, false, false), [b], 0, [b-7], 1, _, _, Accepted),
		assertion(Accepted == false).

	test(tabu_search_tenure_one_blocks_reversal, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0, b-1], none))::run(a, 0, Statistics, [max_steps(4), tabu_tenure(1)]),
		assertion(Statistics == [steps(4), acceptances(2), improvements(0), final_tabu_size(0)]).

	test(tabu_search_tenure_zero, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0, b-1], none))::run(a, 0, Statistics, [max_steps(4), tabu_tenure(0)]),
		assertion(Statistics == [steps(4), acceptances(4), improvements(0), final_tabu_size(0)]).

	test(tabu_search_tenure_two, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0, b-1], none))::run(a, 0, Statistics, [max_steps(4), tabu_tenure(2)]),
		assertion(Statistics == [steps(4), acceptances(2), improvements(0), final_tabu_size(1)]).

	test(tabu_search_tenure_default, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0, b-1], none))::run(a, 0, Statistics, [max_steps(4)]),
		assertion(Statistics == [steps(4), acceptances(1), improvements(0), final_tabu_size(1)]).

	test(tabu_search_tenure_range_overrides_zero, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0, b-1], none))::run(a, 0, Statistics, [max_steps(4), tabu_tenure(0), tabu_tenure_range(1,1)]),
		assertion(Statistics == [steps(4), acceptances(2), improvements(0), final_tabu_size(0)]).

	test(tabu_search_tenure_boundaries, deterministic) :-
		tabu_search(quadratic, xoshiro128pp)<<update_tabu(policy(false, false, false, false, false), a, 5, 0, 0, fixed(2), [], Tabu),
		assertion(Tabu == [a-8]),
		assertion(tabu_search(quadratic, xoshiro128pp)<<is_tabu(a, Tabu, 6)),
		assertion(tabu_search(quadratic, xoshiro128pp)<<is_tabu(a, Tabu, 7)),
		assertion(\+ (tabu_search(quadratic, xoshiro128pp)<<is_tabu(a, Tabu, 8))),
		tabu_search(quadratic, xoshiro128pp)<<prune_tabu(Tabu, 8, []),
		tabu_search(quadratic, xoshiro128pp)<<active_tabu_size(Tabu, 8, 0).

	test(tabu_search_random_tenure_bounds, deterministic) :-
		forall(list::member(Seed, [1,2,3,42,55]), (
			fast_random(xoshiro128pp)::randomize(Seed),
			tabu_search(quadratic, xoshiro128pp)<<update_tabu(policy(false, false, false, false, false), a, 5, 0, 0, range(2,4), [], [a-Expire]),
			assertion(Expire >= 8),
			assertion(Expire =< 10)
		)).

	test(tabu_search_restart_clears_tabu, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0, b-1], none))::run(a, 0, Statistics, [max_steps(2), tabu_tenure(1), restarts(1)]),
		assertion(Statistics == [steps(4), acceptances(2), improvements(0), final_tabu_size(0)]).

	test(tabu_search_unifiable_states_are_distinct, deterministic) :-
		assertion(\+ (tabu_search(quadratic, xoshiro128pp)<<is_tabu(s(Variable), [s(a)-5], 1))),
		assertion(var(Variable)).

	test(tabu_search_variable_identity, deterministic) :-
		assertion(\+ (tabu_search(quadratic, xoshiro128pp)<<is_tabu(s(First), [s(Second)-5], 1))),
		assertion(var(First)),
		assertion(var(Second)),
		tabu_search(quadratic, xoshiro128pp)<<is_tabu(s(First), [s(First)-5], 1),
		assertion(var(First)).

	test(tabu_search_identity_expired_then_active, deterministic) :-
		tabu_search(quadratic, xoshiro128pp)<<is_tabu(a, [a-1,b-5,a-5], 1),
		assertion(\+ (tabu_search(quadratic, xoshiro128pp)<<is_tabu(a, [a-1,b-5], 1))).

	test(tabu_search_nonground_public_state, deterministic) :-
		tabu_search(tabu_search_fixture(full, [s(a)-0,s(b)-0,s(Variable)-0], none))::run(_, _, Statistics, [max_steps(2)]),
		assertion(var(Variable)),
		assertion(Statistics == [steps(2),acceptances(2),improvements(0),final_tabu_size(2)]).

	test(tabu_search_sampling_preserves_variables, deterministic) :-
		forall(list::member(Seed, [1,2,3,42,55,2147483647]), (
			fast_random(xoshiro128pp)::randomize(Seed),
			Input = [s(Variable),s(a),s(b)],
			tabu_search(quadratic, xoshiro128pp)<<sample_list(Input, 3, 2, Sample),
			assertion(var(Variable)),
			length(Sample, 2),
			identity_subset(Sample, Input)
		)).

	test(tabu_search_sampling_boundaries, deterministic) :-
		tabu_search(quadratic, xoshiro128pp)<<sample_list([], 0, 1, []),
		tabu_search(quadratic, xoshiro128pp)<<sample_list([a], 1, 0, []),
		tabu_search(quadratic, xoshiro128pp)<<sample_list([a], 1, 1, [a]),
		tabu_search(quadratic, xoshiro128pp)<<sample_list([a,b], 2, 2, [a,b]),
		tabu_search(quadratic, xoshiro128pp)<<sample_list([a,b], 2, 3, [a,b]).

	test(tabu_search_sampling_repeated_values, deterministic) :-
		fast_random(xoshiro128pp)::randomize(42),
		tabu_search(quadratic, xoshiro128pp)<<sample_list([a,a,b], 3, 2, Sample),
		length(Sample, 2),
		identity_subset(Sample, [a,a,b]).

	test(tabu_search_sampling_repeatable, deterministic(First == Second)) :-
		fast_random(xoshiro128pp)::randomize(42),
		tabu_search(quadratic, xoshiro128pp)<<sample_list([a,b,c,d], 4, 2, First),
		fast_random(xoshiro128pp)::randomize(42),
		tabu_search(quadratic, xoshiro128pp)<<sample_list([a,b,c,d], 4, 2, Second).

	test(tabu_search_progress_completed_steps, deterministic) :-
		Problem = tabu_search_fixture(full, [a-0,b-1,c-2,d-3], none),
		Problem::clear_log,
		tabu_search(Problem)::run(a, 0, [max_steps(3), updates(3), tabu_tenure(0)]),
		Problem::reports(Reports),
		assert_reports(Reports, [report(1,0,1,1,0),report(2,0,2,1,0),report(3,0,3,1,0)]).

	test(tabu_search_progress_interval_rates, deterministic) :-
		Problem = tabu_search_fixture(full, [a-2,b-1,c-1,d-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(3), updates(3), tabu_tenure(0)]),
		Problem::reports(Reports),
		assert_reports(Reports, [report(1,1,1,1,1),report(2,1,1,1,0),report(3,1,1,1,0)]).

	test(tabu_search_progress_stalled_intervals, deterministic) :-
		Problem = tabu_search_fixture(full, [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(3), updates(3)]),
		Problem::reports(Reports),
		assert_reports(Reports, [report(1,0,1,1,0),report(2,0,1,0,0),report(3,0,1,0,0)]).

	test(tabu_search_progress_early_stop, deterministic) :-
		Problem = tabu_search_fixture(full, [a-0,b-1,c-2], 2),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, Statistics, [max_steps(5), updates(1), tabu_tenure(0)]),
		Problem::reports(Reports),
		assert_reports(Reports, [report(2,0,2,1,0)]),
		assertion(Statistics == [steps(2),acceptances(2),improvements(0),final_tabu_size(0)]).

	test(tabu_search_progress_zero_step, deterministic) :-
		Problem = tabu_search_fixture(full, [a-0,b-1], 0),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(3), updates(3)]),
		Problem::reports(Reports),
		assert_reports(Reports, [report(0,0,0,0,0)]).

	test(tabu_search_progress_partial_final_interval, deterministic) :-
		Problem = tabu_search_fixture(full, [a-0,b-1,c-2,d-3,e-4,f-5,g-6,h-7], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(7), updates(2), tabu_tenure(0)]),
		Problem::reports(Reports),
		assert_reports(Reports, [report(3,0,3,1,0),report(6,0,6,1,0),report(7,0,7,1,0)]).

	test(tabu_search_progress_restart_intervals, deterministic) :-
		Problem = tabu_search_fixture(full, [a-0,b-1,c-2], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, Statistics, [max_steps(2), updates(8), restarts(1), tabu_tenure(0)]),
		Problem::reports(Reports),
		assert_reports(Reports, [report(1,0,1,1,0),report(2,0,2,1,0),report(3,0,1,1,0),report(4,0,2,1,0)]),
		assertion(Statistics == [steps(4),acceptances(4),improvements(0),final_tabu_size(0)]).

	test(tabu_search_progress_disabled, deterministic(Reports == [])) :-
		Problem = tabu_search_fixture(full, [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(2), updates(0)]),
		Problem::reports(Reports).

	test(tabu_search_progress_early_boundary, deterministic) :-
		Problem = tabu_search_fixture(full, [a-0,b-1], 1),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(3), updates(3)]),
		Problem::reports(Reports),
		assert_reports(Reports, [report(1,0,1,1,0)]).

	quick_check(tabu_search_sampling_properties,
		sampling_properties(+between(integer, 0, 100), +between(integer, 0, 100), +between(integer, 1, 2147483647)), [n(100)]).

	test(tabu_search_enumerated_candidate_limit, deterministic) :-
		tabu_search(tabu_search_fixture(enumerated, [a-10,b-4,c-3,d-2], none))::run(_, Energy, [max_steps(2), candidates(2), seed(2147483647)]),
		assertion(Energy < 10).

	test(tabu_search_sampling_sparse_dense_middle, deterministic) :-
		forall(list::member(Count, [2,50,98]), sampling_properties(100, Count, 2147483647)).

	test(tabu_search_aspiration_boundaries, deterministic) :-
		tabu_search(quadratic, xoshiro128pp)<<is_admissible(policy(false, false, false, false, false), a, -1, 0, [a-5], 1, true),
		tabu_search(quadratic, xoshiro128pp)<<is_admissible(policy(false, false, false, false, false), a, 0, 0, [a-5], 1, false),
		tabu_search(quadratic, xoshiro128pp)<<is_admissible(policy(false, false, false, false, false), a, 1, 0, [a-5], 1, false),
		tabu_search(quadratic, xoshiro128pp)<<is_admissible(policy(false, false, false, false, false), b, 1, 0, [a-5], 1, true).

	test(tabu_search_candidate_pruning_keeps_worsening_best, deterministic) :-
		tabu_search(tabu_search_fixture(full, [b-3,c-2,d-2], none), xoshiro128pp)<<evaluate_candidates(policy(false, false, false, false, false), [b,c,d], 0, [], 1, State, Energy, Accepted),
		assertion(State == c),
		assertion(Energy == 2),
		assertion(Accepted == true).

	test(tabu_search_inadmissible_first_candidate, deterministic) :-
		tabu_search(tabu_search_fixture(full, [b-1,c-2,d-3], none), xoshiro128pp)<<evaluate_candidates(policy(false, false, false, false, false), [b,c,d], 0, [b-5], 1, State, Energy, Accepted),
		assertion(State == c),
		assertion(Energy == 2),
		assertion(Accepted == true).

	test(tabu_search_active_tabu_counts, deterministic) :-
		tabu_search(quadratic, xoshiro128pp)<<active_tabu_size([], 0, 0),
		tabu_search(quadratic, xoshiro128pp)<<active_tabu_size([a-5,b-6], 1, 2),
		tabu_search(quadratic, xoshiro128pp)<<active_tabu_size([a-1,b-2], 2, 0),
		tabu_search(quadratic, xoshiro128pp)<<active_tabu_size([a-1,b-5,c-2], 2, 1).

	test(tabu_search_fixed_tenure_capacity, deterministic) :-
		check_tenure_capacity(0, 10, fixed(3), [], 3).

	test(tabu_search_ranged_tenure_capacity, deterministic) :-
		check_tenure_capacity(0, 10, range(3,3), [], 3).

	test(tabu_search_run_two_arguments, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0,b-1], 0))::run(a, 0).

	test(tabu_search_alternate_random_algorithm, deterministic) :-
		tabu_search(tabu_search_fixture(enumerated, [a-10,b-4,c-3,d-2], none), as183)::run(_, Energy, [max_steps(2), candidates(2), seed(42)]),
		assertion(Energy < 10).

	test(tabu_search_delta_avoids_energy_recomputation, deterministic) :-
		Problem = tabu_search_fixture(delta, [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(1), candidates(4)]),
		Problem::calls(energy, 1),
		Problem::calls(neighbor, 4).

	test(tabu_search_sampled_energy_call_count, deterministic) :-
		Problem = tabu_search_fixture(counted, [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(1), candidates(4)]),
		Problem::calls(energy, 5),
		Problem::calls(neighbor, 4).

	test(tabu_search_evaluates_nonwinning_energy, deterministic) :-
		Problem = tabu_search_fixture(counted, [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem, xoshiro128pp)<<evaluate_candidates(policy(false, false, false, false, false), [b,b,b], 0, [], 1, b, 1, true),
		Problem::calls(energy, 3).

	test(tabu_search_missing_progress_hook, deterministic) :-
		tabu_search(quadratic)::run(_, _, [max_steps(2), updates(2)]).

	test(tabu_search_failing_progress_hook, deterministic(Reports == [])) :-
		Problem = tabu_search_fixture(no_progress, [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(2), updates(2)]),
		Problem::reports(Reports).

	test(tabu_search_failing_neighbor_hook, fail) :-
		tabu_search(tabu_search_fixture(no_neighbor, [a-0], none), xoshiro128pp)<<generate_neighbor(a, 0, _, _, _).

	test(tabu_search_repeated_options_use_first, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0,b-1], none))::run(_, _, Statistics, [max_steps(1),max_steps(3),tabu_tenure(0),tabu_tenure(2)]),
		assertion(Statistics == [steps(1),acceptances(1),improvements(0),final_tabu_size(0)]).

	test(tabu_search_invalid_updates, error(domain_error(option, updates(-1)))) :-
		tabu_search(quadratic)::run(_, _, [updates(-1)]).

	test(tabu_search_invalid_zero_steps, error(domain_error(option, max_steps(0)))) :-
		tabu_search(quadratic)::run(_, _, [max_steps(0)]).

	test(tabu_search_invalid_range_maximum, error(domain_error(option, tabu_tenure_range(1,0)))) :-
		tabu_search(quadratic)::run(_, _, [tabu_tenure_range(1,0)]).

	test(tabu_search_tsp_improves_initial_tour, deterministic) :-
		tsp::initial_state(Initial),
		tsp::state_energy(Initial, InitialEnergy),
		tabu_search(tsp)::run(Tour, Energy, [seed(42),max_steps(200),candidates(20)]),
		assertion(Energy < InitialEnergy),
		msort(Tour, Sorted),
		msort(Initial, Sorted).

	test(tabu_search_progress_fractional_rates, deterministic) :-
		Problem = tabu_search_fixture(full, [a-3,b-2,c-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(7), updates(2)]),
		Problem::reports(Reports),
		Rate is 2 / 3,
		assert_reports(Reports, [report(3,1,1,Rate,Rate),report(6,1,1,0,0),report(7,1,1,0,0)]).

	test(tabu_search_canonical_key_blocks_equivalent_state, deterministic) :-
		tabu_search(tabu_search_inherited_key_fixture)::run(State, Energy, Statistics, [max_steps(2)]),
		assertion(State == s(a,1)),
		assertion(Energy == 0),
		assertion(Statistics == [steps(2),acceptances(1),improvements(0),final_tabu_size(1)]).

	test(tabu_search_key_compact_storage_and_expiry, deterministic) :-
		Problem = tabu_search_key_fixture(canonical, full, [s(a,large_payload)-0], none),
		tabu_search(Problem, xoshiro128pp)<<update_tabu(policy(true, false, false, false, false), s(a,large_payload), 5, 0, 0, fixed(2), [], Tabu),
		assertion(Tabu == [key(a)-8]),
		assertion(tabu_search(Problem, xoshiro128pp)<<candidate_tabu(policy(true, false, false, false, false), s(a,other), Tabu, 7)),
		assertion(\+ (tabu_search(Problem, xoshiro128pp)<<candidate_tabu(policy(true, false, false, false, false), s(a,other), Tabu, 8))).

	test(tabu_search_key_sampled_path, deterministic) :-
		tabu_search(tabu_search_key_fixture(canonical, sampled, [s(a,1)-0,s(b,1)-1,s(a,2)-0], none))::run(s(a,1), 0, Statistics, [max_steps(2),candidates(1)]),
		assertion(Statistics == [steps(2),acceptances(1),improvements(0),final_tabu_size(1)]).

	test(tabu_search_identity_key_matches_default, deterministic) :-
		tabu_search(tabu_search_fixture(full, [a-0,b-1], none))::run(State, Energy, Statistics, [max_steps(4)]),
		tabu_search(tabu_search_key_fixture(identity, full, [a-0,b-1], none))::run(State, Energy, Statistics, [max_steps(4)]).

	test(tabu_search_key_variable_identity, deterministic) :-
		Problem = tabu_search_key_fixture(canonical, full, [s(Variable,1)-0], none),
		tabu_search(Problem, xoshiro128pp)<<update_tabu(policy(true, false, false, false, false), s(Variable,1), 0, 0, 0, fixed(2), [], Tabu),
		assertion(tabu_search(Problem, xoshiro128pp)<<candidate_tabu(policy(true, false, false, false, false), s(Variable,2), Tabu, 1)),
		assertion(\+ (tabu_search(Problem, xoshiro128pp)<<candidate_tabu(policy(true, false, false, false, false), s(Other,2), Tabu, 1))),
		assertion(var(Variable)),
		assertion(var(Other)).

	test(tabu_search_key_absent_declaration, deterministic) :-
		tabu_search(quadratic, xoshiro128pp)<<hook_available(quadratic, tabu_key(_, _), false),
		tabu_search(quadratic, xoshiro128pp)<<hook_available(tabu_search_inherited_key_fixture, tabu_key(_, _), true).

	test(tabu_search_key_failure, error(domain_error(tabu_hook_result, tabu_key/2))) :-
		tabu_search(tabu_search_key_fixture(fail, full, [a-0,b-1], none))::run(_, _, [max_steps(1)]).

	test(tabu_search_key_unbound_result, error(domain_error(tabu_hook_result, tabu_key/2-_))) :-
		tabu_search(tabu_search_key_fixture(invalid, full, [a-0,b-1], none))::run(_, _, [max_steps(1)]).

	test(tabu_search_key_exception, ball(key_error)) :-
		tabu_search(tabu_search_key_fixture(throw, full, [a-0,b-1], none))::run(_, _, [max_steps(1)]).

	test(tabu_search_key_skipped_when_unneeded, deterministic) :-
		Problem = tabu_search_key_fixture(fail, full, [a-0,b-1], none),
		tabu_search(Problem)::run(a, 0, [max_steps(2),tabu_tenure(0)]),
		tabu_search(Problem, xoshiro128pp)<<is_admissible(policy(true, false, false, false, false), a, -1, 0, [a-5], 1, true).

	test(tabu_search_worse_restart_progress_and_counts, deterministic) :-
		Problem = tabu_search_restart_fixture(value(b), [a-0,b-1], 0),
		Problem::clear_log,
		tabu_search(Problem)::run(a, 0, Statistics, [max_steps(1),restarts(2),updates(3)]),
		Problem::restart_inputs([a,a]),
		Problem::calls(energy, 3),
		Problem::reports(Reports),
		assert_reports(Reports, [report(0,0,0,0,0),report(0,0,1,0,0),report(0,0,1,0,0)]),
		assertion(Statistics == [steps(0),acceptances(0),improvements(0),final_tabu_size(0)]).

	test(tabu_search_equal_restart_preserves_best, deterministic) :-
		tabu_search(tabu_search_restart_fixture(value(b), [a-1,b-1], 0))::run(a, 1, [max_steps(1),restarts(1)]).

	test(tabu_search_identity_restart_and_clear_memory, deterministic) :-
		Problem = tabu_search_restart_fixture(identity, [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(a, 0, Statistics, [max_steps(2),candidates(1),restarts(1),tabu_tenure(1)]),
		Problem::restart_inputs([a]),
		assertion(Statistics == [steps(4),acceptances(2),improvements(0),final_tabu_size(0)]).

	test(tabu_search_restart_not_called_without_restarts, deterministic) :-
		tabu_search(tabu_search_restart_fixture(fail, [a-0,b-1], 0))::run(a, 0, [max_steps(1)]).

	test(tabu_search_restart_input_not_bound, deterministic) :-
		Problem = tabu_search_restart_fixture(identity, [s(Variable)-0], 0),
		tabu_search(Problem)::run(State, 0, [max_steps(1),restarts(1)]),
		assertion(State == s(Variable)),
		assertion(var(Variable)).

	test(tabu_search_restart_failure, error(domain_error(tabu_hook_result, restart_state/2))) :-
		tabu_search(tabu_search_restart_fixture(fail, [a-0], 0))::run(_, _, [max_steps(1),restarts(1)]).

	test(tabu_search_restart_unbound_result, error(domain_error(tabu_hook_result, restart_state/2-_))) :-
		tabu_search(tabu_search_restart_fixture(invalid, [a-0], 0))::run(_, _, [max_steps(1),restarts(1)]).

	test(tabu_search_restart_exception, ball(restart_error)) :-
		tabu_search(tabu_search_restart_fixture(throw, [a-0], 0))::run(_, _, [max_steps(1),restarts(1)]).

	test(tabu_search_restart_seeded_diversification, deterministic) :-
		Problem = tabu_search_restart_fixture(random, [a-0,b-1], none),
		tabu_search(Problem, as183)::run(State, Energy, Statistics, [max_steps(2),candidates(1),restarts(3),seed(42)]),
		tabu_search(Problem, as183)::run(State, Energy, Statistics, [max_steps(2),candidates(1),restarts(3),seed(42)]).

	test(tabu_search_custom_tenure_pre_move_and_mixed_expiration, deterministic) :-
		Problem = tabu_search_tenure_fixture(schedule([4,0,1]), [a-0,b-1,c-2,d-3], none),
		Problem::clear_log,
		tabu_search(Problem)::run(a, 0, Statistics, [max_steps(3)]),
		Problem::tenure_inputs([tenure(0,0,0),tenure(1,0,1),tenure(2,0,2)]),
		assertion(Statistics == [steps(3),acceptances(3),improvements(0),final_tabu_size(2)]).

	test(tabu_search_custom_zero_tenure_retains_active_entries, deterministic) :-
		Problem = tabu_search_tenure_fixture(value(0), [a-0,b-1], none),
		tabu_search(Problem, xoshiro128pp)<<update_tabu(policy(false,false,true,false,false), a, 1, 0, 1, fixed(7), [old-10,expired-2], Tabu),
		assertion(Tabu == [old-10]).

	test(tabu_search_custom_tenure_calls_only_on_acceptance, deterministic) :-
		Problem = tabu_search_tenure_fixture(value(7), [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(3)]),
		Problem::tenure_inputs([tenure(0,0,0)]).

	test(tabu_search_custom_tenure_global_steps_across_restarts, deterministic) :-
		Problem = tabu_search_tenure_fixture(value(1), [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(_, _, [max_steps(2),restarts(1)]),
		Problem::tenure_inputs([tenure(0,0,0),tenure(2,0,0)]).

	test(tabu_search_custom_tenure_no_unused_random_draw, deterministic) :-
		Problem = tabu_search_tenure_fixture(value(1), [a-0,b-1], none),
		tabu_search(Problem, as183)::run(_, _, [max_steps(1),seed(42),tabu_tenure_range(2,9)]),
		fast_random(as183)::between(1, 1000, Actual),
		fast_random(as183)::randomize(42),
		fast_random(as183)::between(1, 1000, Expected),
		assertion(Actual == Expected).

	test(tabu_search_custom_tenure_increasing_schedule, deterministic) :-
		tabu_search(tabu_search_tenure_fixture(schedule([1,2,3]), [a-3,b-2,c-1,d-0], none))::run(d, 0, Statistics, [max_steps(3)]),
		assertion(Statistics == [steps(3),acceptances(3),improvements(3),final_tabu_size(2)]).

	test(tabu_search_custom_tenure_failure, error(domain_error(tabu_hook_result, tabu_tenure/4))) :-
		tabu_search(tabu_search_tenure_fixture(fail, [a-0,b-1], none))::run(_, _, [max_steps(1)]).

	test(tabu_search_custom_tenure_negative, error(domain_error(tabu_hook_result, tabu_tenure/4- -1))) :-
		tabu_search(tabu_search_tenure_fixture(value(-1), [a-0,b-1], none))::run(_, _, [max_steps(1)]).

	test(tabu_search_custom_tenure_float, error(domain_error(tabu_hook_result, tabu_tenure/4-1.5))) :-
		tabu_search(tabu_search_tenure_fixture(value(1.5), [a-0,b-1], none))::run(_, _, [max_steps(1)]).

	test(tabu_search_custom_tenure_atom, error(domain_error(tabu_hook_result, tabu_tenure/4-invalid))) :-
		tabu_search(tabu_search_tenure_fixture(value(invalid), [a-0,b-1], none))::run(_, _, [max_steps(1)]).

	test(tabu_search_custom_tenure_unbound, error(domain_error(tabu_hook_result, tabu_tenure/4-_))) :-
		tabu_search(tabu_search_tenure_fixture(value(_), [a-0,b-1], none))::run(_, _, [max_steps(1)]).

	test(tabu_search_custom_tenure_exception, ball(tenure_error)) :-
		tabu_search(tabu_search_tenure_fixture(throw, [a-0,b-1], none))::run(_, _, [max_steps(1)]).

	test(tabu_search_custom_aspiration_permits_equal_best, deterministic) :-
		tabu_search(tabu_search_inherited_aspiration_fixture)::run(a, 0, Statistics, [max_steps(2)]),
		assertion(Statistics == [steps(2),acceptances(2),improvements(0),final_tabu_size(2)]).

	test(tabu_search_custom_aspiration_rejects_improving_tabu, deterministic) :-
		Problem = tabu_search_aspiration_fixture(reject, full, [a-0,b-1], none),
		tabu_search(Problem, xoshiro128pp)<<is_admissible(policy(false,false,false,true,false), a, -1, 0, [a-5], 1, false),
		tabu_search(Problem, xoshiro128pp)<<is_admissible(policy(false,false,false,true,false), b, 1, 0, [a-5], 1, true).

	test(tabu_search_custom_aspiration_permits_worse_tabu, deterministic) :-
		Problem = tabu_search_aspiration_fixture(permit, full, [a-0,b-1], none),
		tabu_search(Problem, xoshiro128pp)<<is_admissible(policy(false,false,false,true,false), b, 1, 0, [b-5], 1, true).

	test(tabu_search_custom_aspiration_reject_public_sampled, deterministic) :-
		Problem = tabu_search_aspiration_fixture(reject, sampled, [a-0,b-1], none),
		Problem::clear_log,
		tabu_search(Problem)::run(a, 0, Statistics, [max_steps(2),candidates(1)]),
		Problem::aspiration_inputs([aspiration(a,0,0)]),
		assertion(Statistics == [steps(2),acceptances(1),improvements(0),final_tabu_size(1)]).

	test(tabu_search_custom_aspiration_permit_public_sampled, deterministic) :-
		tabu_search(tabu_search_aspiration_fixture(permit, sampled, [a-0,b-1], none))::run(a, 0, Statistics, [max_steps(2),candidates(1)]),
		assertion(Statistics == [steps(2),acceptances(2),improvements(0),final_tabu_size(2)]).

	test(tabu_search_custom_aspiration_skips_nonwinning_candidates, deterministic) :-
		Problem = tabu_search_aspiration_fixture(permit, counted, [b-1], none),
		Problem::clear_log,
		tabu_search(Problem, xoshiro128pp)<<evaluate_candidates(policy(false,false,false,true,false), [b,b,b], 0, [b-5], 1, b, 1, true),
		Problem::aspiration_inputs([aspiration(b,1,0)]),
		Problem::calls(energy, 3).

	test(tabu_search_custom_aspiration_receives_original_keyed_state, deterministic) :-
		tabu_search_keyed_aspiration_fixture::clear_log,
		tabu_search(tabu_search_keyed_aspiration_fixture)::run(s(a,1), 0, [max_steps(2)]),
		tabu_search_keyed_aspiration_fixture::aspiration_inputs([aspiration(s(a,2),0,0)]).

	test(tabu_search_custom_aspiration_preserves_variables, deterministic) :-
		Problem = tabu_search_aspiration_fixture(permit, full, [s(Variable)-0], none),
		tabu_search(Problem, xoshiro128pp)<<is_admissible(policy(false,false,false,true,false), s(Variable), 0, 0, [s(Variable)-5], 1, true),
		assertion(var(Variable)).

	test(tabu_search_custom_aspiration_exception, ball(aspiration_error)) :-
		tabu_search(tabu_search_aspiration_fixture(throw, full, [a-0,b-1], none))::run(_, _, [max_steps(2)]).

	test(tabu_search_custom_aspiration_not_called_for_non_tabu, deterministic) :-
		tabu_search(tabu_search_aspiration_fixture(throw, full, [a-0,b-1], none))::run(a, 0, [max_steps(1)]).

	test(tabu_search_exhaustive_ignores_candidate_limit, deterministic) :-
		tabu_search(tabu_search_fixture(enumerated, [a-10,b-4,c-3,d-2], none))::run(d, 2, [max_steps(1),candidates(1),exhaustive(true)]).

	test(tabu_search_exhaustive_default_false_repeatability, deterministic) :-
		Problem = tabu_search_fixture(enumerated, [a-10,b-4,c-3,d-2], none),
		tabu_search(Problem)::run(State, Energy, Statistics, [max_steps(3),candidates(1),seed(42)]),
		tabu_search(Problem)::run(State, Energy, Statistics, [max_steps(3),candidates(1),seed(42),exhaustive(false)]).

	test(tabu_search_exhaustive_empty_neighborhood, deterministic) :-
		tabu_search(tabu_search_fixture(empty, [a-0], none))::run(a, 0, Statistics, [max_steps(2),exhaustive(true)]),
		assertion(Statistics == [steps(2),acceptances(0),improvements(0),final_tabu_size(0)]).

	test(tabu_search_exhaustive_tie_order, deterministic) :-
		tabu_search(tabu_search_fixture(enumerated, [a-10,c-1,b-1], none))::run(c, 1, [max_steps(1),candidates(1),exhaustive(true)]).

	test(tabu_search_exhaustive_missing_implementation, error(existence_error(procedure, tabu_search_missing_neighborhood_fixture::neighbors/2))) :-
		tabu_search(tabu_search_missing_neighborhood_fixture)::run(_, _, [max_steps(1),exhaustive(true)]).

	test(tabu_search_exhaustive_late_enumeration_failure, error(domain_error(tabu_hook_result, neighbors/2))) :-
		tabu_search(tabu_search_failing_neighborhood_fixture)::run(_, _, [max_steps(2),exhaustive(true)]).

	test(tabu_search_exhaustive_enumeration_exception, ball(neighborhood_error)) :-
		tabu_search(tabu_search_throwing_neighborhood_fixture)::run(_, _, [max_steps(1),exhaustive(true)]).

	test(tabu_search_exhaustive_inherited_implementation, deterministic) :-
		tabu_search(tabu_search_inherited_key_fixture)::run(s(a,1), 0, Statistics, [max_steps(2),exhaustive(true)]),
		assertion(Statistics == [steps(2),acceptances(1),improvements(0),final_tabu_size(1)]).

	test(tabu_search_exhaustive_invalid_atom, error(domain_error(option, exhaustive(invalid)))) :-
		tabu_search(quadratic)::run(_, _, [exhaustive(invalid)]).

	test(tabu_search_exhaustive_invalid_number, error(domain_error(option, exhaustive(1)))) :-
		tabu_search(quadratic)::run(_, _, [exhaustive(1)]).

	test(tabu_search_exhaustive_unbound_option, error(domain_error(option, exhaustive(_)))) :-
		tabu_search(quadratic)::run(_, _, [exhaustive(_)]).

	test(tabu_search_exhaustive_invalid_candidate_limit, error(domain_error(option, candidates(0)))) :-
		tabu_search(quadratic)::run(_, _, [exhaustive(true),candidates(0)]).

	test(tabu_search_exhaustive_repeated_options_first_wins, deterministic) :-
		tabu_search(tabu_search_fixture(enumerated, [a-10,b-4,c-3,d-2], none))::run(d, 2, [max_steps(1),candidates(1),exhaustive(true),exhaustive(false)]),
		tabu_search(tabu_search_missing_neighborhood_fixture, xoshiro128pp)<<require_neighborhood(false, tabu_search_missing_neighborhood_fixture).

	test(tabu_search_exhaustive_no_sampling_random_draw, deterministic) :-
		tabu_search(tabu_search_fixture(enumerated, [a-10,b-4,c-3,d-2], none), as183)::run(d, 2, [max_steps(1),candidates(1),exhaustive(true),tabu_tenure(0),seed(42)]),
		fast_random(as183)::between(1, 1000, Actual),
		fast_random(as183)::randomize(42),
		fast_random(as183)::between(1, 1000, Expected),
		assertion(Actual == Expected).

	test(tabu_search_exhaustive_uses_full_energy_not_delta, deterministic) :-
		tabu_search_enumerated_delta_fixture::clear_log,
		tabu_search(tabu_search_enumerated_delta_fixture)::run(c, 1, [max_steps(1),exhaustive(true)]),
		tabu_search_enumerated_delta_fixture::calls(energy, 3),
		tabu_search_enumerated_delta_fixture::calls(neighbor, 0).

	test(tabu_search_combined_policies_both_generators_and_paths, deterministic) :-
		forall(list::member(Mode, [full,sampled]), (
			forall(list::member(Algorithm, [xoshiro128pp,as183]), (
				tabu_search(tabu_search_combined_fixture(Mode), Algorithm)::run(State, Energy, Statistics, [max_steps(2),candidates(1),restarts(1),seed(42),tabu_tenure_range(7,9)]),
				assertion(State == s(a,2)),
				assertion(Energy == 0),
				assertion(Statistics == [steps(4),acceptances(4),improvements(2),final_tabu_size(1)])
			))
		)).

	test(tabu_search_combined_exhaustive_and_progress, deterministic) :-
		Problem = tabu_search_combined_fixture(full),
		Problem::clear_log,
		tabu_search(Problem)::run(s(a,2), 0, Statistics, [max_steps(2),candidates(1),restarts(1),exhaustive(true),updates(8)]),
		Problem::aspiration_inputs([aspiration(s(a,2),0,1)]),
		Problem::reports(Reports),
		assert_reports(Reports, [report(1,1,1,1,1),report(2,0,0,1,1),report(3,0,0,1,0),report(4,0,2,1,0)]),
		assertion(Statistics == [steps(4),acceptances(4),improvements(2),final_tabu_size(1)]).

	test(tabu_search_combined_repeatability_both_arities, deterministic) :-
		Problem = tabu_search_combined_fixture(full),
		Options = [max_steps(2),candidates(1),restarts(1),seed(42)],
		tabu_search(Problem)::run(State, Energy, Statistics, Options),
		tabu_search(Problem, xoshiro128pp)::run(State, Energy, Statistics, Options),
		tabu_search(Problem, as183)::run(State, Energy, Statistics, Options).

	test(tabu_search_custom_zero_tenure_skips_key, deterministic) :-
		Problem = tabu_search_key_fixture(fail, full, [a-0], none),
		tabu_search(Problem, xoshiro128pp)<<update_tabu(policy(true,false,false,false,false), a, 1, 0, 0, fixed(0), [old-10], [old-10]).

	test(tabu_search_restart_energy_exception_propagates, ball(restart_energy_error)) :-
		tabu_search(tabu_search_restart_energy_error_fixture)::run(_, _, [max_steps(1),restarts(1)]).

	test(tabu_search_category_key_implementation, deterministic) :-
		tabu_search(tabu_search_category_key_fixture)::run(a, 0, Statistics, [max_steps(4),tabu_tenure(1)]),
		assertion(Statistics == [steps(4),acceptances(2),improvements(0),final_tabu_size(0)]),
		tabu_search(quadratic, xoshiro128pp)<<hook_available(tabu_search_category_key_fixture, tabu_key(_, _), true).

	test(tabu_search_custom_tenure_overrides_options, deterministic) :-
		tabu_search(tabu_search_inherited_tenure_fixture)::run(a, 0, Statistics, [max_steps(4),tabu_tenure(0),tabu_tenure_range(7,9)]),
		assertion(Statistics == [steps(4),acceptances(2),improvements(0),final_tabu_size(0)]).

	test(tabu_search_better_restart_updates_best_without_moves, deterministic) :-
		tabu_search(tabu_search_inherited_restart_fixture)::run(State, Energy, Statistics, [max_steps(1),restarts(1)]),
		assertion(State == b),
		assertion(Energy == 1),
		assertion(Statistics == [steps(0),acceptances(0),improvements(0),final_tabu_size(0)]).

	% auxiliary predicates

	identity_subset([], _).
	identity_subset([Head| Tail], Input) :-
		remove_identical(Head, Input, Rest),
		identity_subset(Tail, Rest).

	remove_identical(Element, [Head| Tail], Tail) :-
		Element == Head,
		!.
	remove_identical(Element, [Head| Tail], [Head| Rest]) :-
		remove_identical(Element, Tail, Rest).

	assert_reports([], []).
	assert_reports([report(Step,Best,Current,Acceptance,Improvement)| Reports], [report(ExpectedStep,ExpectedBest,ExpectedCurrent,ExpectedAcceptance,ExpectedImprovement)| Expected]) :-
		assertion(Step == ExpectedStep),
		assertion(Best =:= ExpectedBest),
		assertion(Current =:= ExpectedCurrent),
		assertion(lgtunit::(Acceptance =~= ExpectedAcceptance)),
		assertion(lgtunit::(Improvement =~= ExpectedImprovement)),
		assert_reports(Reports, Expected).

	sampling_properties(Size, Count, Seed) :-
		tabu_search_fixture(full, [a-0], none)::numbers(Size, Input),
		fast_random(xoshiro128pp)::randomize(Seed),
		tabu_search(quadratic, xoshiro128pp)<<sample_list(Input, Size, Count, First),
		Expected is min(Size, Count),
		length(First, Expected),
		identity_subset(First, Input),
		fast_random(xoshiro128pp)::randomize(Seed),
		tabu_search(quadratic, xoshiro128pp)<<sample_list(Input, Size, Count, Second),
		First == Second.

	check_tenure_capacity(Limit, Limit, _, _, _) :-
		!.
	check_tenure_capacity(Step, Limit, Spec, Tabu, Tenure) :-
		tabu_search(quadratic, xoshiro128pp)<<update_tabu(policy(false, false, false, false, false), Step, Step, 0, 0, Spec, Tabu, NextTabu),
		length(NextTabu, Size),
		assertion(Size =< Tenure),
		( Step >= Tenure - 1 ->
			assertion(Size == Tenure)
		; true
		),
		Next is Step + 1,
		check_tenure_capacity(Next, Limit, Spec, NextTabu, Tenure).

:- end_object.
