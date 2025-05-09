%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Default library paths for the programming examples
%  Last updated on March 19, 2025
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


% logtalk_library_path(Library, Path)
%
% paths must always end with a "/"

:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

% programming examples

logtalk_library_path(examples, logtalk_user('examples/')).

logtalk_library_path(ack, examples('ack/')).
logtalk_library_path(activation_nodes, ncl('activation_nodes/')).
logtalk_library_path(adventure, examples('adventure/')).
logtalk_library_path(aliases, examples('aliases/')).
logtalk_library_path(apache_poi, examples('apache_poi/')).
logtalk_library_path(around_methods, examples('around_methods/')).
logtalk_library_path(aspects, examples('aspects/')).
logtalk_library_path(assign_parameters, examples('assign_parameters/')).
logtalk_library_path(assumptions, examples('assumptions/')).
logtalk_library_path(attvars, examples('attvars/')).
logtalk_library_path(backtracking_nodes, ncl('backtracking_nodes/')).
logtalk_library_path(barriers, threads('barriers/')).
logtalk_library_path(bench, examples('bench/')).
logtalk_library_path(benchmarks, examples('benchmarks/')).
logtalk_library_path(birds, examples('birds/')).
logtalk_library_path(birthdays, threads('birthdays/')).
logtalk_library_path(blackboard, threads('blackboard/')).
logtalk_library_path(blocks, examples('blocks/')).
logtalk_library_path(books, examples('books/')).
logtalk_library_path(bottles, examples('bottles/')).
logtalk_library_path(bricks, examples('bricks/')).
logtalk_library_path(buckets, threads('buckets/')).
logtalk_library_path(buffer, threads('buffer/')).
logtalk_library_path(carengines, examples('carengines/')).
logtalk_library_path(cascade, examples('cascade/')).
logtalk_library_path(checkpoint, threads('checkpoint/')).
logtalk_library_path(classmethods, examples('classmethods/')).
logtalk_library_path(classvars, examples('classvars/')).
logtalk_library_path(closed_world_assumption, examples('closed_world_assumption/')).
logtalk_library_path(clp_bp, constraints('bp/')).
logtalk_library_path(clp_eclipse, constraints('eclipse/')).
logtalk_library_path(clp_gprolog, constraints('gprolog/')).
logtalk_library_path(clp_sicstus, constraints('sicstus/')).
logtalk_library_path(clp_swipl, constraints('swipl/')).
logtalk_library_path(clp_yap, constraints('yap/')).
logtalk_library_path(clustering, examples('clustering/')).
logtalk_library_path(coinduction, examples('coinduction/')).
logtalk_library_path(complements_allow, examples('complements/allow/')).
logtalk_library_path(complements_restrict, examples('complements/restrict/')).
logtalk_library_path(constraints, examples('constraints/')).
logtalk_library_path(dcgs, examples('dcgs/')).
logtalk_library_path(debug_hooks, examples('debug_hooks/')).
logtalk_library_path(defaulty, examples('defaulty/')).
logtalk_library_path(delegates, examples('delegates/')).
logtalk_library_path(design_patterns, examples('design_patterns/')).
logtalk_library_path(document_converter, examples('document_converter/')).
logtalk_library_path(diamonds, examples('diamonds/')).
logtalk_library_path(dynpred, examples('dynpred/')).
logtalk_library_path(ebench, engines('ebench/')).
logtalk_library_path(edcgs, examples('edcgs/')).
logtalk_library_path(elephants, examples('elephants/')).
logtalk_library_path(emetas, engines('emetas/')).
logtalk_library_path(encodings, examples('encodings/')).
logtalk_library_path(engines, examples('engines/')).
logtalk_library_path(errors, examples('errors/')).
logtalk_library_path(expansion, examples('expansion/')).
logtalk_library_path(family, examples('family/')).
logtalk_library_path(family_alt, examples('family_alt/')).
logtalk_library_path(fft, threads('fft/')).
logtalk_library_path(fibonacci, threads('fibonacci/')).
logtalk_library_path(figures, ncl('figures/')).
logtalk_library_path(fluents, engines('fluents/')).
logtalk_library_path(free_nodes, ncl('free_nodes/')).
logtalk_library_path(functions, threads('functions/')).
logtalk_library_path(futures, examples('futures/')).
logtalk_library_path(hailstone, examples('hailstone/')).
logtalk_library_path(hanoi, threads('hanoi/')).
logtalk_library_path(haunted_wasteland, examples('haunted_wasteland/')).
logtalk_library_path(hello_world, examples('hello_world/')).
logtalk_library_path(hooks, examples('hooks/')).
logtalk_library_path(includes, examples('includes/')).
logtalk_library_path(inheritance, examples('inheritance/')).
logtalk_library_path(inlining, examples('inlining/')).
logtalk_library_path(instmethods, examples('instmethods/')).
logtalk_library_path(instvars, examples('instvars/')).
logtalk_library_path(integration, threads('integration/')).
logtalk_library_path(integration2d, threads('integration2d/')).
logtalk_library_path(interactors, engines('interactors/')).
logtalk_library_path(jpl, examples('jpl/')).
logtalk_library_path(lambdas, examples('lambdas/')).
logtalk_library_path(lambdas_compiled, examples('lambdas_compiled/')).
logtalk_library_path(laptops, examples('laptops/')).
logtalk_library_path(lazy, engines('lazy/')).
logtalk_library_path(lips, examples('lips/')).
logtalk_library_path(lo_planner, examples('lo/planner/')).
logtalk_library_path(lo_travellers, examples('lo/travellers/')).
logtalk_library_path(localizations, examples('localizations/')).
logtalk_library_path(logic, examples('logic/')).
logtalk_library_path(logs, examples('logs/')).
logtalk_library_path(lpa_faults, examples('lpa/faults/')).
logtalk_library_path(lpa_timetables, examples('lpa/timetables/')).
logtalk_library_path(metaclasses, examples('metaclasses/')).
logtalk_library_path(metainterpreters, examples('metainterpreters/')).
logtalk_library_path(metapredicates, examples('metapredicates/')).
logtalk_library_path(metapredicates_compiled, examples('metapredicates_compiled/')).
logtalk_library_path(metered_concurrency, threads('metered_concurrency/')).
logtalk_library_path(mi, examples('mi/')).
logtalk_library_path(miscellaneous, examples('miscellaneous/')).
logtalk_library_path(missing_data, examples('missing_data/')).
logtalk_library_path(modules, examples('modules/')).
logtalk_library_path(module_aliases, examples('module_aliases/')).
logtalk_library_path(msglog, examples('msglog/')).
logtalk_library_path(mtbatch, threads('mtbatch/')).
logtalk_library_path(multifile, examples('multifile/')).
logtalk_library_path(my_types, examples('my_types/')).
logtalk_library_path(named_databases, examples('named_databases/')).
logtalk_library_path(ncl, examples('ncl/')).
logtalk_library_path(nondet, threads('nondet/')).
logtalk_library_path(neo4j, examples('neo4j/')).
logtalk_library_path(now_you_see_me, examples('now_you_see_me/')).
logtalk_library_path(object_aliases, examples('object_aliases/')).
logtalk_library_path(operators, examples('operators/')).
logtalk_library_path(patches, examples('patches/')).
logtalk_library_path(patching, examples('patching/')).
logtalk_library_path(parametric, examples('parametric/')).
logtalk_library_path(pardicts, examples('pardicts/')).
logtalk_library_path(parvars, examples('parvars/')).
logtalk_library_path(pengines, examples('pengines/')).
logtalk_library_path(people, examples('people/')).
logtalk_library_path(permutations, examples('permutations/')).
logtalk_library_path(persistency, examples('persistency/')).
logtalk_library_path(philosophers, threads('philosophers/')).
logtalk_library_path(ping_pong, threads('ping_pong/')).
logtalk_library_path(planets, examples('planets/')).
logtalk_library_path(pmq, engines('pmq/')).
logtalk_library_path(poem, examples('poem/')).
logtalk_library_path(points, examples('points/')).
logtalk_library_path(polygons, examples('polygons/')).
logtalk_library_path(predicate_lookups, examples('predicate_lookups/')).
logtalk_library_path(primes, threads('primes/')).
logtalk_library_path(process_modeling, examples('process_modeling/')).
logtalk_library_path(profiling, examples('profiling/')).
logtalk_library_path(prototypes, examples('prototypes/')).
logtalk_library_path(proxies, examples('proxies/')).
logtalk_library_path(puzzles, examples('puzzles/')).
logtalk_library_path(questions, examples('questions/')).
logtalk_library_path(quick_check, examples('quick_check/')).
logtalk_library_path(recipes, examples('recipes/')).
logtalk_library_path(reflection, examples('reflection/')).
logtalk_library_path(relations, examples('relations/')).
logtalk_library_path(roles, examples('roles/')).
logtalk_library_path(roots, examples('roots/')).
logtalk_library_path(scopes, examples('scopes/')).
logtalk_library_path(scratchcards, examples('scratchcards/')).
logtalk_library_path(searching, examples('searching/')).
logtalk_library_path(securemp, examples('securemp/')).
logtalk_library_path(self_messages, examples('self_messages/')).
logtalk_library_path(self_vs_super, examples('self_vs_super/')).
logtalk_library_path(self_vs_this, examples('self_vs_this/')).
logtalk_library_path(serialization, examples('serialization/')).
logtalk_library_path(shapes_ch, examples('shapes/ch/')).
logtalk_library_path(shapes_ph, examples('shapes/ph/')).
logtalk_library_path(sicstus, examples('sicstus/')).
logtalk_library_path(slides, examples('slides/')).
logtalk_library_path(sorting, threads('sorting/')).
logtalk_library_path(sums, engines('sums/')).
logtalk_library_path(super_calls, examples('super_calls/')).
logtalk_library_path(symbiosis, examples('symbiosis/')).
logtalk_library_path(sync, threads('sync/')).
logtalk_library_path(symdiff, examples('symdiff/')).
logtalk_library_path(tabling, examples('tabling/')).
logtalk_library_path(tcltk, examples('tcltk/')).
logtalk_library_path(tak, threads('tak/')).
logtalk_library_path(tbbt, engines('tbbt/')).
logtalk_library_path(team, threads('team/')).
logtalk_library_path(testing, examples('testing/')).
logtalk_library_path(tests_dsl, examples('tests_dsl/')).
logtalk_library_path(threads, examples('threads/')).
logtalk_library_path(trebuchet, examples('trebuchet/')).
logtalk_library_path(units, examples('units/')).
logtalk_library_path(viewpoints, examples('viewpoints/')).
logtalk_library_path(whisper, engines('whisper/')).
logtalk_library_path(wrappers, examples('wrappers/')).
logtalk_library_path(xpce, examples('xpce/')).
logtalk_library_path(yield, engines('yield/')).
