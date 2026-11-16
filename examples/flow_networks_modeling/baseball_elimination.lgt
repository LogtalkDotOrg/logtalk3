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


:- object(baseball_elimination).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Baseball elimination with ties for first allowed, using a maximum-flow reduction.',
		see_also is [flow_networks(loader), project_selection, image_segmentation]
	]).

	:- uses(list, [
		memberchk/2, append/3
	]).

	:- public(analyze/4).
	:- mode(analyze(+list(compound), +list(compound), +ground, -compound), one_or_error).
	:- info(analyze/4, [
		comment is 'Analyzes whether the named team can finish tied for first. Teams are ``team(Name, Wins, RemainingGames)``. The schedule lists every unordered pair exactly once as ``game(Team1, Team2, Count)`` with ``Team1 @< Team2``; this single count represents both symmetric schedule entries. Returns ``not_eliminated(Allocations)``, ``eliminated(trivial(Team))``, or ``eliminated(certificate(Teams, Left, Right))``.',
		argnames is ['Teams', 'Schedule', 'Team', 'Result'],
		exceptions is [
			'An input list or record is partial, or a team name is not ground' - instantiation_error,
			'An input is not a list' - type_error(list, 'Input'),
			'A team or game record has an invalid shape' - domain_error(baseball_schedule, 'Term'),
			'A win count, remaining count, or game count is not an integer' - type_error(integer, 'Count'),
			'A count is negative' - domain_error(non_negative_integer, 'Count'),
			'A team name is repeated' - domain_error(unique_teams, 'Team'),
			'A game names an undeclared team' - existence_error(team, 'Team'),
			'A game pair is reversed, repeated, or self-referential' - domain_error(unique_unordered_game_pair, 'Pair'),
			'The schedule omits an unordered team pair' - existence_error(game_pair, 'Pair'),
			'A team remaining count disagrees with the schedule' - consistency_error(team_remaining_games, 'Team', 'Declared', 'Scheduled'),
			'The team under consideration is undeclared' - existence_error(team, 'Team')
		]
	]).

	analyze(Teams, Schedule, Team, Result) :-
		(	ground(Team) ->
			true
		;	throw(error(instantiation_error, context(baseball_elimination::analyze/4, _)))
		),
		validate_teams(Teams, []),
		validate_schedule(Schedule, Teams, []),
		validate_complete_schedule(Teams, Schedule),
		validate_remaining_counts(Teams, Schedule),
		(	memberchk(team(Team, _, _), Teams) ->
			true
		;	throw(error(existence_error(team, Team), context(baseball_elimination::analyze/4, _)))
		),
		memberchk(team(Team, Wins, Remaining), Teams),
		MaximumWins is Wins + Remaining,
		(	trivially_eliminated(Teams, Team, MaximumWins, EliminatingTeam) ->
			Result = eliminated(trivial(EliminatingTeam))
		;	nontrivial_analysis(Teams, Schedule, Team, MaximumWins, Result)
		),
		!.

	validate_teams(Teams, Seen) :-
		(	var(Teams) ->
			throw(error(instantiation_error, context(baseball_elimination::analyze/4, _)))
		;	Teams == [] ->
			true
		;	Teams = [Record| Rest] ->
			(	ground(Record) ->
				(	Record = team(Name, Wins, Remaining) ->
					validate_count(Wins),
					validate_count(Remaining),
					(	memberchk(Name, Seen) ->
						throw(error(domain_error(unique_teams, Name), context(baseball_elimination::analyze/4, _)))
					;	validate_teams(Rest, [Name| Seen])
					)
				;	throw(error(domain_error(baseball_schedule, Record), context(baseball_elimination::analyze/4, _)))
				)
			;	throw(error(instantiation_error, context(baseball_elimination::analyze/4, _)))
			)
		;	throw(error(type_error(list, Teams), context(baseball_elimination::analyze/4, _)))
		).

	validate_schedule(Schedule, Teams, Seen) :-
		(	var(Schedule) ->
			throw(error(instantiation_error, context(baseball_elimination::analyze/4, _)))
		;	Schedule == [] ->
			true
		;	Schedule = [Record| Rest] ->
			(	ground(Record) ->
				(	Record = game(First, Second, Count) ->
					validate_count(Count),
					(	memberchk(team(First, _, _), Teams) ->
						(	memberchk(team(Second, _, _), Teams) ->
							(	First @< Second,
								First \== Second,
								\+ memberchk(game(First, Second, _), Seen) ->
								validate_schedule(Rest, Teams, [Record| Seen])
							;	throw(error(domain_error(unique_unordered_game_pair, First-Second), context(baseball_elimination::analyze/4, _)))
							)
						;	throw(error(existence_error(team, Second), context(baseball_elimination::analyze/4, _)))
						)
					;	throw(error(existence_error(team, First), context(baseball_elimination::analyze/4, _)))
					)
				;	throw(error(domain_error(baseball_schedule, Record), context(baseball_elimination::analyze/4, _)))
				)
			;	throw(error(instantiation_error, context(baseball_elimination::analyze/4, _)))
			)
		;	throw(error(type_error(list, Schedule), context(baseball_elimination::analyze/4, _)))
		).

	validate_count(Count) :-
		(	integer(Count) ->
			(	Count >= 0 ->
				true
			;	throw(error(domain_error(non_negative_integer, Count), context(baseball_elimination::analyze/4, _)))
			)
		;	throw(error(type_error(integer, Count), context(baseball_elimination::analyze/4, _)))
		).

	validate_complete_schedule([], _).
	validate_complete_schedule([team(Name, _, _)| Teams], Schedule) :-
		validate_pairs_for_team(Name, Teams, Schedule),
		validate_complete_schedule(Teams, Schedule).

	validate_pairs_for_team(_, [], _).
	validate_pairs_for_team(Name, [team(Other, _, _)| Teams], Schedule) :-
		ordered_pair(Name, Other, First, Second),
		(	memberchk(game(First, Second, _), Schedule) ->
			validate_pairs_for_team(Name, Teams, Schedule)
		;	throw(error(existence_error(game_pair, First-Second), context(baseball_elimination::analyze/4, _)))
		).

	ordered_pair(First, Second, First, Second) :-
		First @< Second,
		!.
	ordered_pair(First, Second, Second, First).

	validate_remaining_counts([], _).
	validate_remaining_counts([team(Name, _, Remaining)| Teams], Schedule) :-
		scheduled_games(Name, Schedule, 0, Scheduled),
		(	Remaining =:= Scheduled ->
			validate_remaining_counts(Teams, Schedule)
		;	throw(error(consistency_error(team_remaining_games, Name, Remaining, Scheduled), context(baseball_elimination::analyze/4, _)))
		).

	scheduled_games(_, [], Total, Total).
	scheduled_games(Name, [game(First, Second, Count)| Games], Total0, Total) :-
		(	(	Name == First ; Name == Second	) ->
			Total1 is Total0 + Count
		;	Total1 = Total0
		),
		scheduled_games(Name, Games, Total1, Total).

	trivially_eliminated([team(Name, Wins, _)| _], Team, MaximumWins, Name) :-
		Name \== Team,
		Wins > MaximumWins,
		!.
	trivially_eliminated([_| Teams], Team, MaximumWins, EliminatingTeam) :-
		trivially_eliminated(Teams, Team, MaximumWins, EliminatingTeam).

	nontrivial_analysis(Teams, Schedule, Team, MaximumWins, Result) :-
		opponent_teams(Teams, Team, Opponents),
		opponent_games(Schedule, Team, Games),
		total_games(Games, 0, TotalGames),
		Infinity is TotalGames + 1,
		all_team_vertices(Teams, TeamVertices),
		game_vertices(Games, GameVertices),
		vertices_with_terminals(TeamVertices, GameVertices, Vertices),
		game_arcs(Games, Infinity, 1, GameArcs, AllocationMap, NextId),
		team_sink_arcs(Opponents, MaximumWins, NextId, TeamArcs),
		append(GameArcs, TeamArcs, Arcs),
		flow_network(avltree)::new(Vertices, Arcs, Network),
		dinic(avltree)::max_flow(Network, flow_example_source(baseball), flow_example_sink(baseball), Value, Flow),
		(	Value =:= TotalGames ->
			determine_allocation(Schedule, Team, AllocationMap, Flow, Allocations),
			Result = not_eliminated(Allocations)
		;	dinic(avltree)::min_cut(Network, flow_example_source(baseball), flow_example_sink(baseball), _, cut(SourceSide, _, _)),
			certificate_teams(Opponents, SourceSide, CertificateTeams),
			certificate_values(CertificateTeams, Games, 0, SumWins, 0, InternalGames),
			length(CertificateTeams, Count),
			Right is Count * MaximumWins,
			Left is SumWins + InternalGames,
			Left > Right,
			Result = eliminated(certificate(CertificateTeams, Left, Right))
		).

	opponent_teams([], _, []).
	opponent_teams([team(Name, Wins, Remaining)| Teams], Team, Opponents) :-
		opponent_teams(Teams, Team, Rest),
		(	Name == Team ->
			Opponents = Rest
		;	Opponents = [team(Name, Wins, Remaining)| Rest]
		).

	opponent_games([], _, []).
	opponent_games([game(First, Second, Count)| Schedule], Team, Games) :-
		opponent_games(Schedule, Team, Rest),
		(	(	First == Team ; Second == Team	) ->
			Games = Rest
		;	Games = [game(First, Second, Count)| Rest]
		).

	total_games([], Total, Total).
	total_games([game(_, _, Count)| Games], Total0, Total) :-
		Total1 is Total0 + Count,
		total_games(Games, Total1, Total).

	all_team_vertices([], []).
	all_team_vertices([team(Name, _, _)| Teams], [flow_example_team(Name)| Vertices]) :-
		all_team_vertices(Teams, Vertices).

	game_vertices([], []).
	game_vertices([game(First, Second, _)| Games], [flow_example_game(First, Second)| Vertices]) :-
		game_vertices(Games, Vertices).

	vertices_with_terminals(TeamVertices, GameVertices, [flow_example_source(baseball), flow_example_sink(baseball)| Vertices]) :-
		append(TeamVertices, GameVertices, Vertices).

	game_arcs([], _, NextId, [], [], NextId).
	game_arcs([game(First, Second, Count)| Games], Infinity, ArcId, Arcs, [allocation_ids(First, Second, FirstId, SecondId)| Map], NextId) :-
		GameNode = flow_example_game(First, Second),
		FirstId is ArcId + 1,
		SecondId is ArcId + 2,
		NextArcId is ArcId + 3,
		Arcs = [arc(flow_example_source(baseball), GameNode, Count),
			arc(GameNode, flow_example_team(First), Infinity),
			arc(GameNode, flow_example_team(Second), Infinity)| Rest],
		game_arcs(Games, Infinity, NextArcId, Rest, Map, NextId).

	team_sink_arcs([], _, _, []).
	team_sink_arcs([team(Name, Wins, _)| Teams], MaximumWins, ArcId, [arc(flow_example_team(Name), flow_example_sink(baseball), Allowance)| Arcs]) :-
		Allowance is MaximumWins - Wins,
		NextId is ArcId + 1,
		team_sink_arcs(Teams, MaximumWins, NextId, Arcs).

	certificate_teams([], _, []).
	certificate_teams([team(Name, Wins, Remaining)| Teams], SourceSide, CertificateTeams) :-
		certificate_teams(Teams, SourceSide, Rest),
		(	memberchk(flow_example_team(Name), SourceSide) ->
			CertificateTeams = [team(Name, Wins, Remaining)| Rest]
		;	CertificateTeams = Rest
		).

	certificate_values([], _, Wins, Wins, Games, Games).
	certificate_values([team(Name, TeamWins, _)| Teams], AllGames, Wins0, Wins, Games0, Games) :-
		internal_games(Name, Teams, AllGames, 0, TeamGames),
		Wins1 is Wins0 + TeamWins,
		Games1 is Games0 + TeamGames,
		certificate_values(Teams, AllGames, Wins1, Wins, Games1, Games).

	internal_games(_, [], _, Total, Total).
	internal_games(Name, [team(Other, _, _)| Teams], AllGames, Total0, Total) :-
		ordered_pair(Name, Other, First, Second),
		(	memberchk(game(First, Second, Count), AllGames) ->
			Total1 is Total0 + Count
		;	Total1 = Total0
		),
		internal_games(Name, Teams, AllGames, Total1, Total).

	determine_allocation([], _, _, _, []).
	determine_allocation([game(First, Second, Count)| Schedule], Team, Map, Flow, [allocation(First, Second, FirstWins, SecondWins)| Allocations]) :-
		(	First == Team ->
			FirstWins = Count,
			SecondWins = 0
		;	Second == Team ->
			FirstWins = 0,
			SecondWins = Count
		;	memberchk(allocation_ids(First, Second, FirstId, SecondId), Map),
			flow_amount(FirstId, Flow, FirstWins),
			flow_amount(SecondId, Flow, SecondWins),
			FirstWins + SecondWins =:= Count
		),
		determine_allocation(Schedule, Team, Map, Flow, Allocations).

	flow_amount(_, [], 0).
	flow_amount(ArcId, [flow(ArcId0, _, _, Amount)| _], Amount) :-
		ArcId =:= ArcId0,
		!.
	flow_amount(ArcId, [_| Flow], Amount) :-
		flow_amount(ArcId, Flow, Amount).

:- end_object.
