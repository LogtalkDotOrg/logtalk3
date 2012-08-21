%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Simple parser of PDDL files.
%%   Author: Robert Sasak, Charles University in Prague
%%
%% Examples:
%% 
%% ?- pddl::parse_domain('blocks_world.pddl', O).
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
%%        [ action('pick-up', [block(?x)],         %parameters
%%		      [clear(?x), ontable(?x), handempty], %preconditions
%%		      [holding(?x)],                       %positiv effects
%%          [ontable(?x), clear(?x), handempty],   %negativ effects
%%          [increase('total-cost', 2)]),          %numeric effects
%%         ...],
%%       ...)
%%
%% ?-problem::parse_problem('problem.pddl', O).
%%   O = problem('blocks-4-0',						%name
%%              blocks,								%domain name
%%              _G1443,								%require definition
%%              [block(d, b, a, c)],				%object declaration
%%              [ clear(c), clear(a), clear(b), clear(d), ontable(c), %initial state
%%                ontable(a), ontable(b), ontable(d), handempty,
%%                set('total-cost', 0)	],
%%              [on(d, c), on(c, b), on(b, a)],		%goal
%%              _G1447,								%constraints-not implemented
%%              metric(minimize, 'total-cost'),		%metric
%%              _G1449								%length_specification-not implemented
%%              )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(pddl,
	imports(read_file)).

	:- info([
		version is 1.0,
		author is 'Robert Sasak, Charles University in Prague. Adapted to Logtalk by Paulo Moura.',
		date is 2011/08/04,
		comment is 'Simple parser of PDDL 3.0 files.']).

	% Defining operator ?. It is a syntax sugar for marking variables: ?x
	% (commented out as Logtalk already defines a global op(200, fy, ?) operator)
	%:-op(300, fy, ?).

	:- public(parse_domain/3).
	:- mode(parse_domain(+atom, -compound, -list(atom)), one).
	:- info(parse_domain/3,
		[comment is 'Parses a PDDL 3.0 domain file, returning a compound term representing its contents and rest of the file. Useful when domain and problem are in one file.',
		 argnames is ['File', 'Output', 'RestOfFile']]).

	:- public(parse_domain/2).
	:- mode(parse_domain(+atom, -compound), one).
	:- info(parse_domain/2,
		[comment is 'Parses a PDDL 3.0 domain file, returning a compound term representing its contents.',
		 argnames is ['File', 'Output']]).

	parse_domain(File, Output) :-
		parse_domain(File, Output, _).

	parse_domain(File, Output, RestOfFile) :-
		:read_file(File, List),
		phrase(domain(Output), List, RestOfFile).

	:- public(parse_problem/2).
	:- mode(parse_problem(+atom, -compound), one).
	:- info(parse_problem/2,
		[comment is 'Parses a PDDL 3.0 problem file, returning a compound term representing its contents.',
		 argnames is ['File', 'Output']]).

	:- public(parse_problem/3).
	:- mode(parse_problem(+atom, -compound, -list(atom)), one).
	:- info(parse_problem/3,
		[comment is 'Parses a PDDL 3.0 problem file, returning a compound term representing its contents and rest of the file. Useful when domain and problem are in one file.',
		 argnames is ['File', 'Output', 'RestOfFile']]).

	parse_problem(File, Output) :-
		parse_problem(File, Output, _).

	parse_problem(File, Output, RestOfFile) :-
		:read_file(File, List),
		phrase(problem(Output), List, RestOfFile).

	% List of DCG rules describing structure of domain file in language PDDL.
	% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
	% This parser do not fully NOT support PDDL 3.0
	% However you will find comment out lines ready for futher development.
	domain(domain(Name, Requirements, Types, Constants, Predicates, Functions, _, Structure)) -->
		['(', 'define', '(', 'domain'], name(Name), [')'],
		(require_def(Requirements)  ; []),
		(types_def(Types)           ; []),	%:typing
		(constants_def(Constants)   ; []),
		(predicates_def(Predicates) ; []),
		(functions_def(Functions)   ; []),	%:fluents
		%(constraints(Constraints)  ; []),	%:constraints
		zeroOrMore(structure_def, Structure),
		[')'].

	require_def(Requirements) -->
		['(', ':', 'requirements'], oneOrMore(require_key, Requirements), [')'].

	require_key(strips)                      --> [':strips'].
	require_key(typing)                      --> [':typing'].
%	require_key('negative-preconditions')    --> [':negative-preconditions'].
%	require_key('disjunctive-preconditions') --> [':disjunctive-preconditions'].
	require_key(equality)                    --> [':equality'].
	require_key('existential-preconditions') --> [':existential-preconditions'].
	require_key('universal-preconditions')   --> [':universal-preconditions'].
	require_key('quantified-preconditions')  --> [':quantified-preconditions'].
	require_key('conditional-effects')       --> [':conditional-effects'].
	require_key(fluents)                     --> [':fluents'].
	require_key(adl)                         --> [':adl'].
	require_key('durative-actions')          --> [':durative-actions'].
	require_key('derived-predicates')        --> [':derived-predicates'].
	require_key('timed-initial-literals')    --> [':timed-initial-literals'].
	require_key(preferences)                 --> [':preferences'].
	require_key(constraints)                 --> [':constraints'].
	% Universal requirements
	require_key(Requirement)                 --> [':', Requirement].

	types_def(Types) -->
		['(', ':', types], typed_list(name, Types), [')'].

	constants_def(Constants) -->
		['(', ':', constants], typed_list(name, Constants), [')'].

	predicates_def(Predicates) -->
		['(', ':', predicates], oneOrMore(atomic_formula_skeleton, Predicates), [')'].

	atomic_formula_skeleton(Formula) -->
		['('], predicate(Predicate), typed_list(variable, Variables), [')'],
		{Formula =.. [Predicate| Variables]}.

	predicate(Predicate) -->
		name(Predicate).

	variable(Variable) -->
		['?'], name(Name),
		{Variable =.. [?, Name]}.

	atomic_function_skeleton(f(Symbol, Variables)) -->
		['('], function_symbol(Symbol), typed_list(variable, Variables), [')'].

	function_symbol(Symbol) -->
		name(Symbol).

	functions_def(Function) -->
		['(', ':', functions], function_typed_list(atomic_function_skeleton, Function), [')'].	%:fluents

%	constraints(Constraints) -->
%		['(', ':', constraints], con_GD(Constraints), [')'].	%:constraints

	structure_def(Action) -->
		action_def(Action).
%	structure_def(DurativeAction) -->
%		durative_action_def(DurativeAction).	%:durativeactions
%	structure_def(DerivedPredicate)	-->
%		derived_def(DerivedPredicate).			%:derivedpredicates

%	typed_list(W, G) -->
%		oneOrMore(W, N), ['-'], type(T),
%		{G =.. [T, N]}.
	typed_list(W, [G| Ns]) -->
		oneOrMore(W, N), ['-'], type(T), !, typed_list(W, Ns),
		{G =.. [T| N]}.
	typed_list(W, N) -->
		zeroOrMore(W, N).

	primitive_type(Name) -->
		name(Name).

	type(either(PrimitiveTypes)) -->
		['(', either], !, oneOrMore(primitive_type, PrimitiveTypes), [')'].
	type(PrimitiveType) -->
		primitive_type(PrimitiveType).

	function_typed_list(W, [F| Ls])	-->
		oneOrMore(W, L), ['-'], !, function_type(T), function_typed_list(W, Ls),
		{F =.. [T| L]}.	%:typing
	function_typed_list(W, L) -->
		zeroOrMore(W, L).

	function_type(number) --> [number].

	emptyOr(_) --> ['(', ')'].
	emptyOr(W) --> call(W).

	% Actions definitons
	action_def(action(Symbol, Parameters, PreCondition, Pos, Neg, Assign)) -->
		['(', ':', action], action_symbol(Symbol),
		[':', parameters, '('], typed_list(variable, Parameters), [')'],
		action_def_body(PreCondition, Pos, Neg, Assign),
		[')'].

	action_symbol(Name) -->
		name(Name).

	action_def_body(PreCondition, Pos, Neg, Assign) -->
		([':', precondition], emptyOr(pre_GD(PreCondition))		; []),
		([':', effect],       emptyOr(effect(Pos, Neg, Assign))	; []).

	pre_GD(P) -->
		pref_GD(P).
	pre_GD(P) -->
		['(', and], pre_GD(P), [')'].
%	pre_GD(forall(L, P)) -->
%		['(', forall, '('], typed_list(variable, L), [')'], pre_GD(P), [')'].	%:universal-preconditions

%	pref_GD(preference(N, P)) -->
%		['(', preference], (pref_name(N); []), gd(P), [')'].					%:preferences
	pref_GD(P) -->
		gd(P).

	pref_name(Name) -->
		name(Name).

	gd(Formula) -->
		atomic_formula(term, Formula).	%: this option is covered by gd(L)
%	gd(L) -->
%		literal(term, L).													%:negative-preconditions
	gd(P) -->
		['(', and],  zeroOrMore(gd, P), [')'].
%	gd(or(P)) -->
%		['(', or],   zeroOrMore(gd ,P), [')'].								%:disjuctive-preconditions
%	gd(not(P)) -->
%		['(', not],  gd(P), [')'].											%:disjuctive-preconditions
%	gd(imply(P1, P2)) -->
%		['(', imply], gd(P1), gd(P2), [')'].								%:disjuctive-preconditions
%	gd(exists(L, P)) -->
%		['(', exists, '('], typed_list(variable, L), [')'], gd(P), [')'].	%:existential-preconditions
%	gd(forall(L, P)) -->
%		['(', forall, '('], typed_list(variable, L), [')'], gd(P), [')'].	%:universal-preconditions
	gd(F) -->
		f_comp(F).	%:fluents

	f_comp(compare(Operator, Expression1, Expression2)) -->
		['('], binary_comp(Operator), f_exp(Expression1), f_exp(Expression2), [')'].

	literal(Type, Formula) -->
		atomic_formula(Type, Formula).
	literal(Type, not(Formula)) -->
		['(', not], atomic_formula(Type, Formula), [')'].

	atomic_formula(Type, Formula) -->
		['('], predicate(Predicate), zeroOrMore(Type, Terms), [')'],
		{Formula =.. [Predicate| Terms]}.

	term(Name) -->
		name(Name).
	term(Variable) -->
		variable(Variable).

	f_exp(Number) -->
		number(Number).
	f_exp(op(Operator, Expression1, Expression2)) -->
		['('], binary_op(Operator), f_exp(Expression1), f_exp(Expression2), [')'].
	f_exp('-'(Expression)) -->
		['(', '-'], f_exp(Expression), [')'].
	f_exp(Head) -->
		f_head(Head).

	f_head(Function) -->
		['('], function_symbol(Symbol), zeroOrMore(term, Terms), [')'],
		{Function =.. [Symbol| Terms]}.
	f_head(Symbol) -->
		function_symbol(Symbol).

	binary_op(Operator) --> multi_op(Operator).
	binary_op('-')      --> ['-'].
	binary_op('/')      --> ['/'].

	multi_op('*') --> ['*'].
	multi_op('+') --> ['+'].

	binary_comp('>')  --> ['>'].
	binary_comp('<')  --> ['<'].
	binary_comp('=')  --> ['='].
	binary_comp('>=') --> ['>='].
	binary_comp('<=') --> ['<='].

	number(Number) -->
		[Number],
		{number(Number)}.

	% Name is everything that is not number, bracket or question mark.
	% Those rules are not necessary, but rapidly speed up parsing process.
	name(Name) --> [Name], {number(Name), !, fail}.
	name(Name) --> [Name], {Name = (')'), !, fail}.
	name(Name) --> [Name], {Name = ('('), !, fail}.
	name(Name) --> [Name], {Name = ('?'), !, fail}.
	name(Name) --> [Name].

	effect(Pos, Neg, Assign) -->
		['(', and], c_effect(Pos, Neg, Assign), [')'].
	effect(Pos, Neg, Assign) -->
		c_effect(Pos, Neg, Assign).

%	c_effect(forall(E)) -->
%		['(', forall, '('], typed-list(variable)*) effect(E), [')'].	%:conditional-effects
%	c_effect(when(P, E)) -->
%		['(', when], gd(P), cond_effect(E), [')'].						%:conditional-effects
	c_effect(Pos, Neg, Assign) -->
		p_effect(Pos, Neg, Assign).

	p_effect([], [], []) -->
		[].
	p_effect(Ps, Ns, [F| Assign]) -->
		['('], assign_op(Operator), f_head(Head), f_exp(Expression), [')'], p_effect(Ps, Ns, Assign),
		{F =.. [Operator, Head, Expression]}.
	p_effect(Ps, [Formula| Ns], Assign) -->
		['(', not], atomic_formula(term, Formula), [')'], p_effect(Ps, Ns, Assign).
	p_effect([Formula| Ps], Ns, Assign) -->
		atomic_formula(term, Formula), p_effect(Ps, Ns, Assign).
%	p_effect(op(Operator, Head, Expression)) -->
%		%:fluents , What is difference between rule 3 lines above???
%		['('], assign_op(Operator), f_head(Head), f_exp(Expression), [')'].

%	cond_effect(Effect) -->
%		['(', and], zeroOrMore(p_effect, Effect), [')'].	%:conditional-effects
%	cond_effect(Effect) -->
%		p_effect(Effect).									%:conditional-effects

	assign_op(assign)     --> [assign].
	assign_op(scale_up)   --> [scale_up].
	assign_op(scale_down) --> [scale_down].
	assign_op(increase)   --> [increase].
	assign_op(decrease)   --> [decrease].

	% List of DCG rules describing structure of problem file in language PDDL.
	% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
	% This parser do not fully NOT support PDDL 3.0
	% However you will find comment out lines ready for futher development.

	problem(problem(Name, Domain, Requirements, ObjectDeclaration, Init, Goal, _, MetricSpec, LengthSpec)) --> 
		['(', define, '(', problem, Name, ')', '(', ':', domain, Domain, ')'],
		(require_def(Requirements)             ; []),
		(object_declaration(ObjectDeclaration) ; []),
		init(Init),
		goal(Goal),
%		(constraints(Constraints)              ; []), %:constraints
		(metric_spec(MetricSpec)               ; []),
		(length_spec(LengthSpec)               ; []),
		[')'].

	object_declaration(L) -->
		['(', ':', objects], typed_list(name, L), [')'].

	init(Init) -->
		['(', ':', init], zeroOrMore(init_el, Init), [')'].

	init_el(I) -->
		literal(name, I).
	init_el(set(Head, Number)) -->
		['(', '='], f_head(Head), number(Number), [')'].		%fluents
	init_el(at(Number, L)) -->
		['(', at], number(Number), literal(name, L), [')'].		% timed-initial literal

	goal(Goal) -->
		['(', ':', goal], pre_GD(Goal), [')'].

%	constraints(Constraints) -->
%		['(', ':', constraints], pref_con_GD(Constraints), [')'].					%:constraints

	pref_con_GD(and(P)) -->
		['(', and], zeroOrMore(pref_con_GD, P), [')'].
%	pref_con_GD(forall(L, P)) -->
%		['(', forall, '('], typed_list(variable, L), [')'], pref_con_GD(P), [')'].	%universal-preconditions
%	pref_con_GD(prefernce(N, P)) -->
%		['(', preference], (pref_name(N) ; []), con_GD(P), [')'].					%preferences
	pref_con_GD(P) -->
		con_GD(P).

	con_GD(and(L)) -->
		['(', and], zeroOrMore(con_GD, L), [')'].
	con_GD(forall(L, P)) -->
		['(', forall, '('], typed_list(variable, L), [')'], con_GD(P), [')'].
	con_GD(at_end(P)) -->
		['(', at, end], gd(P), [')'].
	con_GD(always(P)) -->
		['(', always], gd(P), [')'].
	con_GD(sometime(P)) -->
		['(', sometime], gd(P), [')'].
	con_GD(within(N, P)) -->
		['(', within], number(N), gd(P), [')'].

	con_GD(at_most_once(P)) -->
		['(', 'at-most-once'], gd(P), [')'].
	con_GD(some_time_after(P1, P2)) -->
		['(', 'sometime-after'], gd(P1), gd(P2), [')'].
	con_GD(some_time_before(P1, P2)) -->
		['(', 'sometime-before'], gd(P1), gd(P2), [')'].
	con_GD(always_within(N, P1, P2)) -->
		['(', 'always-within'], number(N), gd(P1), gd(P2), [')'].
	con_GD(hold_during(N1, N2, P)) -->
		['(', 'hold-during'], number(N1), number(N2), gd(P), [')'].
	con_GD(hold_after(N, P)) -->
		['(', 'hold-after'], number(N), gd(P), [')'].

	metric_spec(metric(Optimization, Expression)) -->
		['(', ':', metric], optimization(Optimization), metric_f_exp(Expression), [')'].

	optimization(minimize) --> [minimize].
	optimization(maximize) --> [maximize].

	metric_f_exp(Expression) -->
		['('], binary_op(Operator), metric_f_exp(Expression1), metric_f_exp(Expression2), [')'],
		{Expression =.. [Operator, Expression1, Expression2]}.
	metric_f_exp(multi_op(Operator, [Expression1| Expressions])) -->
		% I don't see meanful of this rule, in additional is missing in f-exp
		['('], multi_op(Operator), metric_f_exp(Expression1), oneOrMore(metric_f_exp, Expressions), [')'].
	metric_f_exp(Expression) -->
		['(', '-'], metric_f_exp(Expression1), [')'],
		{Expression =.. [-, Expression1]}.
	metric_f_exp(Number) -->
		number(Number).
	metric_f_exp(Function) -->
		['('], function_symbol(Symbol), zeroOrMore(name, Names), [')'],
		{atomic_list_concat([Symbol| Names], '-', Function)}.
	metric_f_exp(function(Symbol)) -->
		function_symbol(Symbol).
	metric_f_exp(total_time) -->
		['total-time'].
	metric_f_exp(is_violated(N)) -->
		['(', 'is-violated'], pref_name(N), [')'].

	% Workaround
	length_spec([]) --> [not_defined].	% there is no definition???

	% BNF operator <term>*
	zeroOrMore(W, R)  --> oneOrMore(W, R).
	zeroOrMore(_, []) --> [].

	% BNF description include operator <term>+ to mark zero or more replacements.
	% This DCG extension to overcome this. 
	oneOrMore(W, [R| Rs]) --> call(W, R), oneOrMore(W, Rs).
	oneOrMore(_, [])      --> [].

	:- if(\+ predicate_property(atomic_list_concat(_,_,_), built_in)).

		atomic_list_concat([E |Es], S, A) :-
			atomic_list_concat(Es, S, E, A).
	
		atomic_list_concat([], _, A, A).
		atomic_list_concat([E |Es], S, A0, A) :-
			atom_concat(A0, S, A1),
			(	atom(E) ->
				atom_concat(A1, E, A2)
			;	number(E) ->
				number_codes(E, Cs),
				atom_codes(Ea, Cs),
				atom_concat(A1, Ea, A2)
			;	fail
			),
			atomic_list_concat(Es, S, A2, A).

	:- endif.

:- end_object.
