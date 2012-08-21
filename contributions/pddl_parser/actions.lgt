
:- category(actions).

	:- info([
		version is 1.0,
		author is 'Robert Sasak, Charles University in Prague. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/12,
		comment is 'Actions grammar rules used for parsing both PDDL 3.0 domains and problems.']).

	:- protected([
		action_def//1,
		pre_GD//1,
		pref_GD//1,
		gd//1,
		f_comp//1,
		literal//2,
		atomic_formula//2,
%		term//1,
		f_exp//1,
		f_head//1,
		binary_op//1,
		multi_op//1,
%		binary_comp//1,
		number//1,
		function_symbol//1,
		name//1,
		predicate//1,
		pref_name//1,
%		primitive_type//1,
		type//1,
		variable//1
	]).

	:- meta_non_terminal(emptyOr(0)).

	% Actions definitons
	action_def(action(S, L, Precon, Pos, Neg, Assign)) -->
		['(', ':', action], action_symbol(S),
		[':', parameters, '('], ::typed_list(variable, L), [')'],
		action_def_body(Precon, Pos, Neg, Assign),
		[')'].

	action_symbol(N)		--> name(N).

	action_def_body(P, Pos, Neg, Assign) -->
		(([':', precondition], emptyOr(pre_GD(P)))	; []),
		(([':', effect],       emptyOr(effect(Pos, Neg, Assign)))	; []).

	effect(P, N, A)			--> ['(', and], c_effect(P, N, A), [')'].
	effect(P, N, A)			--> c_effect(P, N, A).

	%c_effect(forall(E))	--> ['(',forall,'('], typed-list(variable)∗) effect(E), ')'.	%:conditional-effects
	%c_effect(when(P, E))	--> ['(',when], :gd(P), cond_effect(E), [')'].					%:conditional-effects
	c_effect(P, N, A)		--> p_effect(P, N, A).

	p_effect([], [], [])		--> [].
	p_effect(Ps, Ns, [F|As]) 	-->
		['('], assign_op(O), f_head(H), f_exp(E), [')'], p_effect(Ps, Ns, As), {F =.. [O, H, E]}.
	p_effect(Ps, [F|Ns], As)	--> ['(', not], atomic_formula(term,F), [')'], p_effect(Ps, Ns, As).
	p_effect([F|Ps], Ns, As)	--> atomic_formula(term, F), p_effect(Ps, Ns, As).
	%p_effect(op(O, H, E))		--> ['('], assign_op(O), f_head(H), f_exp(E), [')'].	%:fluents , What is difference between rule 3 lines above???

	%cond_effect(E)			--> ['(', and], ::zeroOrMore(p_effect, E), [')'].				%:conditional-effects
	%cond_effect(E)			--> p_effect(E).											%:conditional-effects

	assign_op(assign)		--> [assign].
	assign_op(scale_up)		--> [scale_up].
	assign_op(scale_down)	--> [scale_down].
	assign_op(increase)		--> [increase].
	assign_op(decrease)		--> [decrease].

	term(N)					--> name(N).
	term(V)					--> variable(V).

	variable(V)				--> ['?'], name(N), {V =.. [?, N]}.

	f_head(F)				--> ['('], function_symbol(S), ::zeroOrMore(term, T), [')'], { F =.. [S| T] }.
	f_head(S)				--> function_symbol(S).

	binary_op(O)			--> multi_op(O).
	binary_op('-')			--> ['−'].
	binary_op('/')			--> ['/'].

	function_symbol(S)		--> name(S).

	emptyOr(_)				--> ['(', ')'].
	emptyOr(W)				--> call(W).

	multi_op('*')			--> ['*'].
	multi_op('+')			--> ['+'].

	% Name is everything that is not number, bracket or question mark.
	% Those rules are not necessary, but rapidly speed up parsing process.
	name(N)					--> [N], {integer(N), !, fail}.
	name(N)					--> [N], {float(N), !, fail}.
	name(N)					--> [N], {N = (')'), !, fail}.
	name(N)					--> [N], {N = ('('), !, fail}.
	name(N)					--> [N], {N = ('?'), !, fail}.
	name(N)					--> [N].

	pre_GD(P)				--> pref_GD(P).
	pre_GD(P)				--> ['(', and], pre_GD(P), [')'].
	%pre_GD(forall(L, P))		--> ['(', forall, '('], ::typed_list(variable, L), [')'], pre_GD(P), [')'].		%:universal-preconditions
	%pref_GD(preference(N, P))	--> ['(', preference], (pref_name(N); []), gd(P), [')'].				%:preferences

	pref_GD(P)				--> gd(P).

	pref_name(N)			--> name(N).

	gd(F)					--> atomic_formula(term, F).	%: this option is covered by gd(L)
	%gd(L)					--> literal(term, L).													%:negative-preconditions
	gd(P)					--> ['(', and],  ::zeroOrMore(gd, P), [')'].
	%gd(or(P))				--> ['(', or],   ::zeroOrMore(gd ,P), [')'].								%:disjuctive-preconditions
	%gd(not(P))				--> ['(', not],  gd(P), [')'].											%:disjuctive-preconditions
	%gd(imply(P1, P2))		--> ['(', imply], gd(P1), gd(P2), [')'].									%:disjuctive-preconditions
	%gd(exists(L, P))		--> ['(', exists, '('], ::typed_list(variable, L), [')'], gd(P), [')'].		%:existential-preconditions
	%gd(forall(L, P))		--> ['(', forall, '('], ::typed_list(variable, L), [')'], gd(P), [')'].		%:universal-preconditions
	gd(F)					--> f_comp(F).	%:fluents

	f_comp(compare(C, E1, E2))	--> ['('], binary_comp(C), f_exp(E1), f_exp(E2), [')'].

	binary_comp('>')		--> ['>'].
	binary_comp('<')		--> ['<'].
	binary_comp('=')		--> ['='].
	binary_comp('>=')		--> ['>='].
	binary_comp('<=')		--> ['<='].

	f_exp(N)				--> number(N).
	f_exp(op(O, E1, E2))	--> ['('], binary_op(O), f_exp(E1), f_exp(E2), [')'].
	f_exp('-'(E))			--> ['(', '-'], f_exp(E), [')'].
	f_exp(H)				--> f_head(H).

	number(N)				--> [N], {integer(N)}.
	number(N)				--> [N], {float(N)}.

	literal(T, F)			--> atomic_formula(T, F).
	literal(T, not(F))		--> ['(', not], atomic_formula(T, F), [')'].

	atomic_formula(_, F)	--> ['('], predicate(P), ::zeroOrMore(term, T), [')'], {F =.. [P| T]}.	% cheating, maybe wrong

	predicate(P)			--> name(P).

	primitive_type(N)		--> name(N).

	type(either(PT))		--> ['(', either], !, ::oneOrMore(primitive_type, PT), [')'].
	type(PT)				--> primitive_type(PT).

:- end_category.
