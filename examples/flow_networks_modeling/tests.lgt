%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Tests and independent exhaustive oracles for the flow-network reduction examples.'
	]).

	:- uses(list, [
		memberchk/2, select/3
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(project_selection, [
		solve/4 as project_solve/4
	]).

	:- uses(image_segmentation, [
		solve/5 as segmentation_solve/5
	]).

	:- uses(baseball_elimination, [
		analyze/4 as baseball_analyze/4
	]).

	test(project_selection_matches_all_subsets, deterministic(Weight =:= Oracle)) :-
		Projects = [
			project(prepare, 4), project(build, 6), project(maintenance, -3),
			project(optional, 0), project(research, 2)
		],
		Dependencies = [
			requires(prepare, maintenance), requires(build, prepare),
			requires(maintenance, build), requires(research, optional)
		],
		project_solve(Projects, Dependencies, Selected, Weight),
		closure_oracle(Projects, Dependencies, Oracle),
		closure_feasible(Selected, Dependencies).

	test(project_selection_zero_and_isolated, deterministic(Selected == [profitable])) :-
		project_solve([project(profitable, 3), project(idle, 0), project(loss, -2)], [], Selected, 3).

	test(project_selection_dependency_tradeoff, deterministic(Weight == 0)) :-
		project_solve([project(gain, 5), project(cost, -5)], [requires(gain, cost)], _, Weight).

	test(project_selection_unknown_dependency, error(existence_error(project, missing))) :-
		project_solve([project(p, 1)], [requires(p, missing)], _, _).

	test(project_selection_rejects_noninteger_weight, error(type_error(integer, 1.5))) :-
		project_solve([project(p, 1.5)], [], _, _).

	test(project_selection_rejects_nonground_id, error(instantiation_error)) :-
		project_solve([project(_, 1)], [], _, _).

	test(image_segmentation_matches_all_labelings, deterministic(Energy =:= Oracle)) :-
		Pixels = [pixel(1,1), pixel(1,2), pixel(2,1), pixel(2,2)],
		Unary = [
			unary(pixel(1,1), 0, 3), unary(pixel(1,2), 2, 0),
			unary(pixel(2,1), 1, 2), unary(pixel(2,2), 3, 0)
		],
		Neighbors = [
			neighbor(pixel(1,1), pixel(1,2), 2),
			neighbor(pixel(1,1), pixel(2,1), 1),
			neighbor(pixel(1,2), pixel(2,2), 2),
			neighbor(pixel(2,1), pixel(2,2), 3)
		],
		segmentation_solve(Pixels, Unary, Neighbors, Labels, Energy),
		segmentation_oracle(Pixels, Unary, Neighbors, Oracle),
		labels_are_binary(Pixels, Labels).

	test(image_segmentation_zero_penalties_preserves_pixels, deterministic(Labels == [p-background,q-background])) :-
		segmentation_solve([p,q], [unary(p,0,0), unary(q,0,0)], [], Labels, 0).

	test(image_segmentation_missing_unary, error(existence_error(unary_penalty, q))) :-
		segmentation_solve([p,q], [unary(p,1,0)], [], _, _).

	test(image_segmentation_rejects_negative_penalty, error(domain_error(non_negative_integer, -1))) :-
		segmentation_solve([p], [unary(p,-1,0)], [], _, _).

	test(baseball_trivial_elimination, deterministic(Result == eliminated(trivial(b)))) :-
		Teams = [team(a, 2, 0), team(b, 3, 0)],
		Schedule = [game(a,b,0)],
		baseball_analyze(Teams, Schedule, a, Result),
		oracle_baseball_feasible(Teams, Schedule, a, false).

	test(baseball_feasible_schedule_matches_oracle, deterministic(Oracle == true)) :-
		Teams = [team(a,2,2), team(b,2,2), team(c,1,2)],
		Schedule = [game(a,b,1), game(a,c,1), game(b,c,1)],
		baseball_analyze(Teams, Schedule, a, not_eliminated(Allocation)),
		oracle_baseball_feasible(Teams, Schedule, a, Oracle),
		valid_allocations(Schedule, Allocation),
		all_target_games_awarded_to_target(Allocation, a),
		all_opponents_within_max(Teams, a, Allocation).

	test(baseball_nontrivial_certificate, deterministic(Left > Right)) :-
		Teams = [team(a,6,0), team(b,6,1), team(c,6,1)],
		Schedule = [game(a,b,0), game(a,c,0), game(b,c,1)],
		baseball_analyze(Teams, Schedule, a, eliminated(certificate(CertificateTeams, Left, Right))),
		CertificateTeams = [team(b,6,1), team(c,6,1)],
		Left =:= 13,
		Right =:= 12,
		oracle_baseball_feasible(Teams, Schedule, a, false).

	test(baseball_rejects_inconsistent_remaining_count, error(consistency_error(team_remaining_games, a, 2, 1))) :-
		baseball_analyze([team(a,0,2), team(b,0,1)], [game(a,b,1)], a, _).

	test(baseball_rejects_noncanonical_pair, error(domain_error(unique_unordered_game_pair, b-a))) :-
		baseball_analyze([team(a,0,1), team(b,0,1)], [game(b,a,1)], a, _).

	test(baseball_rejects_negative_game_count, error(domain_error(non_negative_integer, -1))) :-
		baseball_analyze([team(a,0,0), team(b,0,0)], [game(a,b,-1)], a, _).

	% Independent exhaustive closure oracle.

	closure_oracle(Projects, Dependencies, Best) :-
		findall(Weight,
			(	project_subset(Projects, Selected),
				closure_feasible(Selected, Dependencies),
				closure_weight(Projects, Selected, Weight)
			),
			Weights),
		maximum(Weights, Best).

	project_subset([], []).
	project_subset([project(_, _)| Projects], Selected) :-
		project_subset(Projects, Selected).
	project_subset([project(Id, _)| Projects], [Id| Selected]) :-
		project_subset(Projects, Selected).

	closure_feasible(Selected, Dependencies) :-
		closure_feasible(Selected, Selected, Dependencies).

	closure_feasible([], _, _).
	closure_feasible([Project| Selected], AllSelected, Dependencies) :-
		dependencies_satisfied(Dependencies, Project, AllSelected),
		closure_feasible(Selected, AllSelected, Dependencies).

	dependencies_satisfied([], _, _).
	dependencies_satisfied([requires(Project, Prerequisite)| Dependencies], Selected, Selection) :-
		(	Project == Selected ->
			memberchk(Prerequisite, Selection)
		;	true
		),
		dependencies_satisfied(Dependencies, Selected, Selection).

	closure_weight([], _, 0).
	closure_weight([project(Id, Weight)| Projects], Selected, Total) :-
		closure_weight(Projects, Selected, Rest),
		(	memberchk(Id, Selected) ->
			Total is Rest + Weight
		;	Total = Rest
		).

	maximum([Value| Values], Maximum) :-
		maximum_values(Values, Value, Maximum).

	maximum_values([], Maximum, Maximum).
	maximum_values([Value| Values], Current, Maximum) :-
		Next is max(Value, Current),
		maximum_values(Values, Next, Maximum).

	% Directly evaluate every foreground subset, rather than trusting a cut.

	segmentation_oracle(Pixels, Unary, Neighbors, Minimum) :-
		findall(Energy,
			(	project_pixel_subset(Pixels, Foreground),
				segmentation_energy(Unary, Neighbors, Foreground, Energy)
			),
			Energies),
		minimum(Energies, Minimum).

	project_pixel_subset([], []).
	project_pixel_subset([_| Pixels], Foreground) :-
		project_pixel_subset(Pixels, Foreground).
	project_pixel_subset([Pixel| Pixels], [Pixel| Foreground]) :-
		project_pixel_subset(Pixels, Foreground).

	segmentation_energy(Unary, Neighbors, Foreground, Energy) :-
		unary_energy(Unary, Foreground, 0, UnaryEnergy),
		neighbor_energy(Neighbors, Foreground, UnaryEnergy, Energy).

	unary_energy([], _, Total, Total).
	unary_energy([unary(Pixel, Background, Foreground)| Unary], ForegroundPixels, Total0, Total) :-
		(	memberchk(Pixel, ForegroundPixels) ->
			Total1 is Total0 + Foreground
		;	Total1 is Total0 + Background
		),
		unary_energy(Unary, ForegroundPixels, Total1, Total).

	neighbor_energy([], _, Total, Total).
	neighbor_energy([neighbor(First, Second, Penalty)| Neighbors], Foreground, Total0, Total) :-
		(	(	memberchk(First, Foreground), \+ memberchk(Second, Foreground)
			;	\+ memberchk(First, Foreground), memberchk(Second, Foreground)
			) ->
			Total1 is Total0 + Penalty
		;	Total1 = Total0
		),
		neighbor_energy(Neighbors, Foreground, Total1, Total).

	minimum([Value| Values], Minimum) :-
		minimum_values(Values, Value, Minimum).

	minimum_values([], Minimum, Minimum).
	minimum_values([Value| Values], Current, Minimum) :-
		Next is min(Value, Current),
		minimum_values(Values, Next, Minimum).

	labels_are_binary([], _).
	labels_are_binary([Pixel| Pixels], Labels) :-
		memberchk(Pixel-Label, Labels),
		(	Label == foreground ->
			true
		;	Label == background
		),
		labels_are_binary(Pixels, Labels).

	% Enumerate integer allocations to each opponent game and check the
	% resulting individual win limits independently of the flow network.

	oracle_baseball_feasible(Teams, Schedule, Team, Feasible) :-
		memberchk(team(Team, Wins, Remaining), Teams),
		Maximum is Wins + Remaining,
		opponent_limits(Teams, Team, Maximum, Limits),
		(	allocate_games(Schedule, Team, Limits, []) ->
			Feasible = true
		;	Feasible = false
		),
		!.

	opponent_limits([], _, _, []).
	opponent_limits([team(Name, Wins, _)| Teams], Team, Maximum, Limits) :-
		opponent_limits(Teams, Team, Maximum, Rest),
		(	Name == Team ->
			Limits = Rest
		;	Limit is Maximum - Wins,
			Limits = [Name-Limit| Rest]
		).

	allocate_games([], _, Limits, Wins) :-
		wins_within_limits(Limits, Wins).
	allocate_games([game(First, Second, Count)| Games], Team, Limits, Wins0) :-
		(	First == Team ->
			allocate_games(Games, Team, Limits, Wins0)
		;	Second == Team ->
			allocate_games(Games, Team, Limits, Wins0)
		;	between(0, Count, FirstWins),
			SecondWins is Count - FirstWins,
			add_wins(First, FirstWins, Wins0, Wins1),
			add_wins(Second, SecondWins, Wins1, Wins2),
			allocate_games(Games, Team, Limits, Wins2)
		).

	add_wins(_, 0, Wins, Wins) :-
		!.
	add_wins(Team, Amount, Wins0, Wins) :-
		(	select(Team-Current, Wins0, Rest) ->
			Updated is Current + Amount,
			Wins = [Team-Updated| Rest]
		;	Wins = [Team-Amount| Wins0]
		).

	wins_within_limits([], _).
	wins_within_limits([Team-Limit| Limits], Wins) :-
		(	memberchk(Team-Amount, Wins) ->
			Amount =< Limit
		;	0 =< Limit
		),
		wins_within_limits(Limits, Wins).

	valid_allocations([], []).
	valid_allocations([game(First, Second, Count)| Games], [allocation(First, Second, FirstWins, SecondWins)| Allocations]) :-
		FirstWins >= 0,
		SecondWins >= 0,
		FirstWins + SecondWins =:= Count,
		valid_allocations(Games, Allocations).

	all_target_games_awarded_to_target([], _).
	all_target_games_awarded_to_target([allocation(First, Second, FirstWins, SecondWins)| Allocations], Team) :-
		(	First == Team ->
			FirstWins >= 0,
			SecondWins =:= 0
		;	Second == Team ->
			FirstWins =:= 0,
			SecondWins >= 0
		;	true
		),
		all_target_games_awarded_to_target(Allocations, Team).

	all_opponents_within_max(Teams, Team, Allocations) :-
		memberchk(team(Team, Wins, Remaining), Teams),
		Maximum is Wins + Remaining,
		all_opponent_limits(Teams, Team, Maximum, Allocations).

	all_opponent_limits([], _, _, _).
	all_opponent_limits([team(Name, Wins, _)| Teams], Team, Maximum, Allocations) :-
		(	Name == Team ->
			true
		;	allocation_wins(Allocations, Name, 0, Added),
			Wins + Added =< Maximum
		),
		all_opponent_limits(Teams, Team, Maximum, Allocations).

	allocation_wins([], _, Total, Total).
	allocation_wins([allocation(First, Second, FirstWins, SecondWins)| Allocations], Name, Total0, Total) :-
		(	Name == First ->
			Total1 is Total0 + FirstWins
		;	Name == Second ->
			Total1 is Total0 + SecondWins
		;	Total1 = Total0
		),
		allocation_wins(Allocations, Name, Total1, Total).

:- end_object.
