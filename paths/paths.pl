%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Default library paths 
%  Last updated on September 23, 2015
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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

% when the LOGTALKHOME or the LOGTALKUSER environment variables are not
% defined (we may be e.g. embedding Logtalk in a compiled application),
% assume the current directory as their value

:- initialization((
	% Logtalk startup directory
	(	'$lgt_environment_variable'('LOGTALK_STARTUP_DIRECTORY', _) ->
		LOGTALK_STARTUP_DIRECTORY = '$LOGTALK_STARTUP_DIRECTORY/'
	;	'$lgt_current_directory'(LOGTALK_STARTUP_DIRECTORY0),
		(	sub_atom(LOGTALK_STARTUP_DIRECTORY0, _, _, 0, '/') ->
			LOGTALK_STARTUP_DIRECTORY = LOGTALK_STARTUP_DIRECTORY0
		;	atom_concat(LOGTALK_STARTUP_DIRECTORY0, '/', LOGTALK_STARTUP_DIRECTORY)
		)
	),
	assertz(logtalk_library_path(startup, LOGTALK_STARTUP_DIRECTORY)),
	% Logtalk installation directory
	(	'$lgt_environment_variable'('LOGTALKHOME', _) ->
		LOGTALKHOME = '$LOGTALKHOME/'
	;	'$lgt_current_directory'(LOGTALKHOME0),
		(	sub_atom(LOGTALKHOME0, _, _, 0, '/') ->
			LOGTALKHOME = LOGTALKHOME0
		;	atom_concat(LOGTALKHOME0, '/', LOGTALKHOME)
		)
	),
	assertz(logtalk_library_path(logtalk_home, LOGTALKHOME)),
	% Logtalk user directory
	(	'$lgt_environment_variable'('LOGTALKUSER', _) ->
		LOGTALKUSER = '$LOGTALKUSER/'
	;	'$lgt_current_directory'(LOGTALKUSER0),
		(	sub_atom(LOGTALKUSER0, _, _, 0, '/') ->
			LOGTALKUSER = LOGTALKUSER0
		;	atom_concat(LOGTALKUSER0, '/', LOGTALKUSER)
		)
	),
	assertz(logtalk_library_path(logtalk_user, LOGTALKUSER))
)).

% user home directory
logtalk_library_path(home, HOME) :-
	(	'$lgt_environment_variable'('HOME', _) ->
		% likely a POSIX system but Windows users
		% may also define this environment variable
		HOME = '$HOME/'
	;	'$lgt_environment_variable'('USERPROFILE', _) ->
		% Windows systems define this environment variable
		HOME = '$USERPROFILE/'
	;	fail
	).

logtalk_library_path(core, logtalk_home('core/')).

logtalk_library_path(contributions, logtalk_user('contributions/')).
logtalk_library_path(examples, logtalk_user('examples/')).
logtalk_library_path(library, logtalk_user('library/')).
logtalk_library_path(tools, logtalk_user('tools/')).

logtalk_library_path(flags, contributions('flags/')).
logtalk_library_path(iso8601, contributions('iso8601/')).
logtalk_library_path(pddl_parser, contributions('pddl_parser/')).
logtalk_library_path(verdi_neruda, contributions('verdi_neruda/')).
logtalk_library_path(xml_parser, contributions('xml_parser/')).

logtalk_library_path(assertions, tools('assertions/')).
logtalk_library_path(debugger, tools('debugger/')).
logtalk_library_path(diagrams, tools('diagrams/')).
logtalk_library_path(help, tools('help/')).
logtalk_library_path(lgtdoc, tools('lgtdoc/')).
logtalk_library_path(lgtunit, tools('lgtunit/')).
logtalk_library_path(ports, tools('ports/')).
logtalk_library_path(profiler, tools('profiler/')).

logtalk_library_path(ack, examples('ack/')).
logtalk_library_path(adventure, examples('adventure/')).
logtalk_library_path(aliases, examples('aliases/')).
logtalk_library_path(assignvars, examples('assignvars/')).
logtalk_library_path(assumptions, examples('assumptions/')).
logtalk_library_path(attvars, examples('attvars/')).
logtalk_library_path(barriers, threads('barriers/')).
logtalk_library_path(benchmarks, examples('benchmarks/')).
logtalk_library_path(birds, examples('birds/')).
logtalk_library_path(birthdays, threads('birthdays/')).
logtalk_library_path(blackboard, threads('blackboard/')).
logtalk_library_path(bottles, examples('bottles/')).
logtalk_library_path(bricks, examples('bricks/')).
logtalk_library_path(buckets, threads('buckets/')).
logtalk_library_path(buffer, threads('buffer/')).
logtalk_library_path(cc, examples('cc/')).
logtalk_library_path(checkpoint, threads('checkpoint/')).
logtalk_library_path(classmethods, examples('classmethods/')).
logtalk_library_path(classvars, examples('classvars/')).
logtalk_library_path(clp_bp, constraints('bp/')).
logtalk_library_path(clp_eclipse, constraints('eclipse/')).
logtalk_library_path(clp_gprolog, constraints('gprolog/')).
logtalk_library_path(clp_sicstus, constraints('sicstus/')).
logtalk_library_path(clp_swipl, constraints('swipl/')).
logtalk_library_path(clp_yap, constraints('yap/')).
logtalk_library_path(coinduction, examples('coinduction/')).
logtalk_library_path(complements_allow, examples('complements/allow/')).
logtalk_library_path(complements_restrict, examples('complements/restrict/')).
logtalk_library_path(constraints, examples('constraints/')).
logtalk_library_path(dcgs, examples('dcgs/')).
logtalk_library_path(debug_hooks, examples('debug_hooks/')).
logtalk_library_path(delegates, examples('delegates/')).
logtalk_library_path(diamonds, examples('diamonds/')).
logtalk_library_path(dynpred, examples('dynpred/')).
logtalk_library_path(encodings, examples('encodings/')).
logtalk_library_path(engines, examples('engines/')).
logtalk_library_path(errors, examples('errors/')).
logtalk_library_path(expansion, examples('expansion/')).
logtalk_library_path(family, examples('family/')).
logtalk_library_path(fft, threads('fft/')).
logtalk_library_path(fibonacci, threads('fibonacci/')).
logtalk_library_path(functions, threads('functions/')).
logtalk_library_path(hailstone, examples('hailstone/')).
logtalk_library_path(hanoi, threads('hanoi/')).
logtalk_library_path(hello_world, examples('hello_world/')).
logtalk_library_path(hooks, examples('hooks/')).
logtalk_library_path(includes, examples('includes/')).
logtalk_library_path(inheritance, examples('inheritance/')).
logtalk_library_path(instmethods, examples('instmethods/')).
logtalk_library_path(instvars, examples('instvars/')).
logtalk_library_path(integration, threads('integration/')).
logtalk_library_path(integration2d, threads('integration2d/')).
logtalk_library_path(jpl, examples('jpl/')).
logtalk_library_path(lambdas, examples('lambdas/')).
logtalk_library_path(lambdas_compiled, examples('lambdas_compiled/')).
logtalk_library_path(lo_planner, examples('lo/planner/')).
logtalk_library_path(lo_travellers, examples('lo/travellers/')).
logtalk_library_path(localizations, examples('localizations/')).
logtalk_library_path(logging, examples('logging/')).
logtalk_library_path(logic, examples('logic/')).
logtalk_library_path(lpa_faults, examples('lpa/faults/')).
logtalk_library_path(lpa_timetables, examples('lpa/timetables/')).
logtalk_library_path(metaclasses, examples('metaclasses/')).
logtalk_library_path(metainterpreters, examples('metainterpreters/')).
logtalk_library_path(metapredicates, examples('metapredicates/')).
logtalk_library_path(metapredicates_compiled, examples('metapredicates_compiled/')).
logtalk_library_path(metered_concurrency, threads('metered_concurrency/')).
logtalk_library_path(mi, examples('mi/')).
logtalk_library_path(miscellaneous, examples('miscellaneous/')).
logtalk_library_path(modules, examples('modules/')).
logtalk_library_path(msglog, examples('msglog/')).
logtalk_library_path(mtbatch, threads('mtbatch/')).
logtalk_library_path(multifile, examples('multifile/')).
logtalk_library_path(named_databases, examples('named_databases/')).
logtalk_library_path(nondet, threads('nondet/')).
logtalk_library_path(operators, examples('operators/')).
logtalk_library_path(patches, examples('patches/')).
logtalk_library_path(patching, examples('patching/')).
logtalk_library_path(parametric, examples('parametric/')).
logtalk_library_path(pardicts, examples('pardicts/')).
logtalk_library_path(people, examples('people/')).
logtalk_library_path(philosophers, threads('philosophers/')).
logtalk_library_path(poem, examples('poem/')).
logtalk_library_path(points, examples('points/')).
logtalk_library_path(polygons, examples('polygons/')).
logtalk_library_path(primes, threads('primes/')).
logtalk_library_path(profiling, examples('profiling/')).
logtalk_library_path(prototypes, examples('prototypes/')).
logtalk_library_path(proxies, examples('proxies/')).
logtalk_library_path(puzzles, examples('puzzles/')).
logtalk_library_path(recipes, examples('recipes/')).
logtalk_library_path(reflection, examples('reflection/')).
logtalk_library_path(relations, examples('relations/')).
logtalk_library_path(roots, examples('roots/')).
logtalk_library_path(searching, examples('searching/')).
logtalk_library_path(securemp, examples('securemp/')).
logtalk_library_path(shapes_ch, examples('shapes/ch/')).
logtalk_library_path(shapes_ph, examples('shapes/ph/')).
logtalk_library_path(sicstus, examples('sicstus/')).
logtalk_library_path(sorting, threads('sorting/')).
logtalk_library_path(symbiosis, examples('symbiosis/')).
logtalk_library_path(sync, threads('sync/')).
logtalk_library_path(symdiff, examples('symdiff/')).
logtalk_library_path(tabling, examples('tabling/')).
logtalk_library_path(tak, threads('tak/')).
logtalk_library_path(team, threads('team/')).
logtalk_library_path(testing, examples('testing/')).
logtalk_library_path(threads, examples('threads/')).
logtalk_library_path(viewpoints, examples('viewpoints/')).
logtalk_library_path(wrappers, examples('wrappers/')).
logtalk_library_path(xpce, examples('xpce/')).
