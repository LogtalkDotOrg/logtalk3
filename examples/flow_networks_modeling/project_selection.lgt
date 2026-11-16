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


:- object(project_selection).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Maximum-weight closure and project selection by minimum cut.',
		see_also is [flow_networks(loader), image_segmentation, baseball_elimination]
	]).

	:- uses(list, [
		memberchk/2, append/3
	]).

	:- public(solve/4).
	:- mode(solve(+list(compound), +list(compound), -list(ground), -integer), one_or_error).
	:- info(solve/4, [
		comment is 'Selects a maximum-weight feasible project closure. Projects are ``project(Id, IntegerWeight)`` and dependencies are ``requires(Project, Prerequisite)``. A dependency means that selecting the first project requires selecting the second. ``Selected`` is in input order; ties may return any optimum.',
		argnames is ['Projects', 'Dependencies', 'Selected', 'Weight'],
		exceptions is [
			'An input list or record is partial, or an identifier is not ground' - instantiation_error,
			'An input is not a list' - type_error(list, 'Input'),
			'A project or dependency has an invalid shape' - domain_error(project_selection_input, 'Term'),
			'A project weight is not an integer' - type_error(integer, 'Weight'),
			'A project identifier is repeated' - domain_error(unique_project_ids, 'Id'),
			'A dependency names an undeclared project' - existence_error(project, 'Id')
		]
	]).

	solve(Projects, Dependencies, Selected, Weight) :-
		validate_projects(Projects, []),
		validate_dependencies(Dependencies, Projects),
		sum_positive_weights(Projects, 0, PositiveTotal),
		Infinity is PositiveTotal + 1,
		project_vertices(Projects, ProjectVertices),
		vertices_with_terminals(ProjectVertices, Vertices),
		project_arcs(Projects, Dependencies, Infinity, Arcs),
		flow_network(avltree)::new(Vertices, Arcs, Network),
		dinic(avltree)::min_cut(Network, flow_example_source(project_selection), flow_example_sink(project_selection), Cut, cut(SourceSide, _, _)),
		selected_projects(Projects, SourceSide, Selected),
		DecodedWeight is PositiveTotal - Cut,
		selected_weight(Projects, Selected, Weight),
		Weight =:= DecodedWeight.

	validate_projects(Projects, Seen) :-
		(	var(Projects) ->
			throw(error(instantiation_error, context(project_selection::solve/4, _)))
		;	Projects == [] ->
			true
		;	Projects = [Project| Rest] ->
			(	ground(Project) ->
				(	Project = project(Id, Weight) ->
					(	integer(Weight) ->
						(	memberchk(Id, Seen) ->
							throw(error(domain_error(unique_project_ids, Id), context(project_selection::solve/4, _)))
						;	validate_projects(Rest, [Id| Seen])
						)
					;	throw(error(type_error(integer, Weight), context(project_selection::solve/4, _)))
					)
				;	throw(error(domain_error(project_selection_input, Project), context(project_selection::solve/4, _)))
				)
			;	throw(error(instantiation_error, context(project_selection::solve/4, _)))
			)
		;	throw(error(type_error(list, Projects), context(project_selection::solve/4, _)))
		).

	validate_dependencies(Dependencies, Projects) :-
		(	var(Dependencies) ->
			throw(error(instantiation_error, context(project_selection::solve/4, _)))
		;	Dependencies == [] ->
			true
		;	Dependencies = [Dependency| Rest] ->
			(	ground(Dependency) ->
				(	Dependency = requires(Project, Prerequisite) ->
					(	project_exists(Project, Projects) ->
						(	project_exists(Prerequisite, Projects) ->
							validate_dependencies(Rest, Projects)
						;	throw(error(existence_error(project, Prerequisite), context(project_selection::solve/4, _)))
						)
					;	throw(error(existence_error(project, Project), context(project_selection::solve/4, _)))
					)
				;	throw(error(domain_error(project_selection_input, Dependency), context(project_selection::solve/4, _)))
				)
			;	throw(error(instantiation_error, context(project_selection::solve/4, _)))
			)
		;	throw(error(type_error(list, Dependencies), context(project_selection::solve/4, _)))
		).

	project_exists(Id, [project(Id0, _)| _]) :-
		Id == Id0,
		!.
	project_exists(Id, [_| Projects]) :-
		project_exists(Id, Projects).

	sum_positive_weights([], Total, Total).
	sum_positive_weights([project(_, Weight)| Projects], Total0, Total) :-
		(	Weight > 0 ->
			Total1 is Total0 + Weight
		;	Total1 = Total0
		),
		sum_positive_weights(Projects, Total1, Total).

	project_vertices([], []).
	project_vertices([project(Id, _)| Projects], [flow_example_project(Id)| Vertices]) :-
		project_vertices(Projects, Vertices).

	vertices_with_terminals(ProjectVertices, [flow_example_source(project_selection), flow_example_sink(project_selection)| ProjectVertices]).

	project_arcs(Projects, Dependencies, Infinity, Arcs) :-
		project_weight_arcs(Projects, WeightArcs),
		dependency_arcs(Dependencies, Infinity, DependencyArcs),
		append(WeightArcs, DependencyArcs, Arcs).

	project_weight_arcs([], []).
	project_weight_arcs([project(Id, Weight)| Projects], Arcs) :-
		project_weight_arcs(Projects, Rest),
		Node = flow_example_project(Id),
		(	Weight > 0 ->
			Arcs = [arc(flow_example_source(project_selection), Node, Weight)| Rest]
		;	Weight < 0 ->
			Loss is -Weight,
			Arcs = [arc(Node, flow_example_sink(project_selection), Loss)| Rest]
		;	Arcs = Rest
		).

	dependency_arcs([], _, []).
	dependency_arcs([requires(Project, Prerequisite)| Dependencies], Infinity, [arc(flow_example_project(Project), flow_example_project(Prerequisite), Infinity)| Arcs]) :-
		dependency_arcs(Dependencies, Infinity, Arcs).

	selected_projects([], _, []).
	selected_projects([project(Id, _)| Projects], SourceSide, Selected) :-
		selected_projects(Projects, SourceSide, Rest),
		(	memberchk(flow_example_project(Id), SourceSide) ->
			Selected = [Id| Rest]
		;	Selected = Rest
		).

	selected_weight([], _, 0).
	selected_weight([project(Id, ProjectWeight)| Projects], Selected, Weight) :-
		selected_weight(Projects, Selected, Rest),
		(	memberchk(Id, Selected) ->
			Weight is Rest + ProjectWeight
		;	Weight = Rest
		).

:- end_object.
