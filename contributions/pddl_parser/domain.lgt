%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseDomain.pl
%%   Simple parser of PDDL domain file into prolog syntax.
%% Author: Robert Sasak, Charles University in Prague
%%
%% Example:
%% ?-domain::parse('blocks_world.pddl', O).
%%   O = domain(blocks,
%%        [strips, typing, 'action-costs'],
%%        [block],
%%        _G4108,
%%        [ on(block(?x), block(?y)),
%%	         ontable(block(?x)),
%%	         clear(block(?x)),
%%	         handempty,
%%	         holding(block(?x)) ],
%%        [number(f('total-cost', []))],
%%        _G4108,
%%        [ action('pick-up', [block(?x)],       %parameters
%%		      [clear(?x), ontable(?x), handempty], %preconditions
%%		      [holding(?x)],                       %positiv effects
%%          [ontable(?x), clear(?x), handempty], %negativ effects
%%          [increase('total-cost', 2)]),        %numeric effects
%%         ...],
%%       ...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(domain,
	imports(read_file, common, actions),
	extends(parser)).

	:- info([
		version is 1.0,
		author is 'Robert Sasak, Charles University in Prague. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/12,
		comment is 'Simple parser of PDDL 3.0 domain files.'
	]).

	% Defining operator ?. It is a syntax sugar for marking variables: ?x
	% (commented out as Logtalk already defines a global op(200, fy, ?) operator)
	%:-op(300, fy, ?).

	:- protected([
		atomic_formula_skeleton//1,
		atomic_function_skeleton//1,
		domain//1,
		require_key//1,
		structure_def//1
	]).

	parse(File, Output, RestOfFile) :-
		:read_file(File, List),
		phrase(domain(Output), List, RestOfFile).

	% List of DCG rules describing structure of domain file in language PDDL.
	% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
	% This parser do not fully NOT support PDDL 3.0
	% However you will find comment out lines ready for futher development.
	domain(domain(N, R, T, C, P, F, C, S)) -->
		['(', 'define', '(', 'domain'], :name(N), [')'],
		(:require_def(R)	; []),
		(types_def(T)    	; []),	%:typing
		(constants_def(C) 	; []),
		(predicates_def(P)	; []),
		(functions_def(F)	; []),	%:fluents
		%(constraints(C)	; []),	%:constraints
		:zeroOrMore(structure_def, S),
		[')'].

	require_key(strips)							--> [':strips'].
	require_key(typing)							--> [':typing'].
	%require_key('negative-preconditions')		--> [':negative-preconditions'].
	%require_key('disjunctive-preconditions')	--> [':disjunctive-preconditions'].
	require_key(equality)						--> [':equality'].
	require_key('existential-preconditions')	--> [':existential-preconditions'].
	require_key('universal-preconditions')		--> [':universal-preconditions'].
	require_key('quantified-preconditions')		--> [':quantified-preconditions'].
	require_key('conditional-effects')			--> [':conditional-effects'].
	require_key(fluents)						--> [':fluents'].
	require_key(adl)							--> [':adl'].
	require_key('durative-actions')				--> [':durative-actions'].
	require_key('derived-predicates')			--> [':derived-predicates'].
	require_key('timed-initial-literals')		--> [':timed-initial-literals'].
	require_key(preferences)					--> [':preferences'].
	require_key(constraints)					--> [':constraints'].
	% Universal requirements
	require_key(R)								--> [':', R].

	types_def(L)				--> ['(', ':', types],      :typed_list(::name, L), [')'].

	constants_def(L)			--> ['(', ':', constants],  :typed_list(::name, L), [')'].

	predicates_def(P)			--> ['(', ':', predicates], :oneOrMore(atomic_formula_skeleton, P), [')'].

	atomic_formula_skeleton(F)	--> ['('], :predicate(P), :typed_list(::variable, L), [')'], {F =.. [P|L]}.

	functions_def(F)			--> ['(', ':', functions], function_typed_list(atomic_function_skeleton, F), [')'].	%:fluents

	atomic_function_skeleton(f(S, L)) --> ['('], :function_symbol(S), :typed_list(::variable, L), [')'].

	%constraints(C)				--> ['(', ':', constraints], con_GD(C), [')'].	%:constraints

	structure_def(A)		--> :action_def(A).
	%structure_def(D)		--> :durative_action_def(D).	%:durativeactions
	%structure_def(D)		--> derived_def(D).			%:derivedpredicates

	function_typed_list(W, [F|Ls])	-->	:oneOrMore(W, L), ['-'], !, function_type(T), function_typed_list(W, Ls), {F =.. [T|L]}.	%:typing
	function_typed_list(W, L)		--> :zeroOrMore(W, L).

	function_type(number)	--> [number].

:- end_object.

