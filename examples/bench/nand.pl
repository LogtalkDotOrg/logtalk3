%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This is a rough approximation to the algorithm presented in:
%
%	"An Algorithm for NAND Decomposition Under Network Constraints,"
%	IEEE Trans. Comp., vol C-18, no. 12, Dec. 1969, p. 1098
%	by E. S. Davidson.
%
%  Written by Bruce Holmer
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  I have used the paper's terminology for names used in the program.
%
%  The data structure for representing functions and variables is
%		function(FunctionNumber, TrueSet, FalseSet,
%			ConceivableInputs,
%			ImmediatePredecessors, ImmediateSuccessors,
%			Predecessors, Successors)
%
%
%  Common names used in the program:
%
%	NumVars		number of variables (signal inputs)
%	NumGs		current number of variables and functions
%	Gs		list of variable and function data
%	Gi,Gj,Gk,Gl	individual variable or function--letter corresponds to
%			the subscript in the paper (most of the time)
%	Vector,V	vector from a function's true set
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

top:-main(0).

main(N) :-
	init_state(N, NumVars, NumGs, Gs),
        add_necessary_functions(NumVars, NumGs, Gs, NumGs2, Gs2),
	test_bounds(NumVars, NumGs2, Gs2),
	search(NumVars, NumGs2, Gs2).
main(_).
%	write('Search completed'), nl.

%  Test input
%  init_state(circuit(NumInputs, NumOutputs, FunctionList))
init_state(0, 2, 3, [		% 2 input xor
		function(2, [1,2], [0,3], [], [], [], [], []),
		function(1, [2,3], [0,1], [], [], [], [], []),
		function(0, [1,3], [0,2], [], [], [], [], [])
		]) :-
	update_bounds(_, 100, _).
init_state(1, 3, 4, [		% carry circuit
		function(3, [3,5,6,7], [0,1,2,4], [], [], [], [], []),
		function(2, [4,5,6,7], [0,1,2,3], [], [], [], [], []),
		function(1, [2,3,6,7], [0,1,4,5], [], [], [], [], []),
		function(0, [1,3,5,7], [0,2,4,6], [], [], [], [], [])
		]) :-
	update_bounds(_, 100, _).
init_state(2, 3, 4, [		% example in paper
		function(3, [1,2,4,6,7], [0,3,5], [], [], [], [], []),
		function(2, [4,5,6,7], [0,1,2,3], [], [], [], [], []),
		function(1, [2,3,6,7], [0,1,4,5], [], [], [], [], []),
		function(0, [1,3,5,7], [0,2,4,6], [], [], [], [], [])
		]) :-
	update_bounds(_, 100, _).
init_state(3, 3, 4, [		% sum (3 input xor)
		function(3, [1,2,4,7], [0,3,5,6], [], [], [], [], []),
		function(2, [4,5,6,7], [0,1,2,3], [], [], [], [], []),
		function(1, [2,3,6,7], [0,1,4,5], [], [], [], [], []),
		function(0, [1,3,5,7], [0,2,4,6], [], [], [], [], [])
		]) :-
	update_bounds(_, 100, _).
init_state(4, 3, 5, [		% do sum and carry together
		function(4, [3,5,6,7], [0,1,2,4], [], [], [], [], []),
		function(3, [1,2,4,7], [0,3,5,6], [], [], [], [], []),
		function(2, [4,5,6,7], [0,1,2,3], [], [], [], [], []),
		function(1, [2,3,6,7], [0,1,4,5], [], [], [], [], []),
		function(0, [1,3,5,7], [0,2,4,6], [], [], [], [], [])
		]) :-
	update_bounds(_, 100, _).
init_state(5, 5, 8, [		% 2 bit full adder
		function(7,		% A2 (output)
			[1,3,4,6,9,11,12,14,16,18,21,23,24,26,29,31],
			[0,2,5,7,8,10,13,15,17,19,20,22,25,27,28,30],
			[], [], [], [], []),
		function(6,		% B2 (output)
			[2,3,5,6,8,9,12,15,17,18,20,21,24,27,30,31],
			[0,1,4,7,10,11,13,14,16,19,22,23,25,26,28,29],
			[], [], [], [], []),
		function(5,		% carry-out (output)
			[7,10,11,13,14,15,19,22,23,25,26,27,28,29,30,31],
			[0,1,2,3,4,5,6,8,9,12,16,17,18,20,21,24],
			[], [], [], [], []),
		function(4,		% carry-in
			[16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31],
			[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15],
			[], [], [], [], []),
		function(3,		% B1 input
			[8,9,10,11,12,13,14,15,24,25,26,27,28,29,30,31],
			[0,1,2,3,4,5,6,7,16,17,18,19,20,21,22,23],
			[], [], [], [], []),
		function(2,		% B0 input
			[4,5,6,7,12,13,14,15,20,21,22,23,28,29,30,31],
			[0,1,2,3,8,9,10,11,16,17,18,19,24,25,26,27],
			[], [], [], [], []),
		function(1, 		% A1 input
			[2,3,6,7,10,11,14,15,18,19,22,23,26,27,30,31],
			[0,1,4,5,8,9,12,13,16,17,20,21,24,25,28,29],
			[], [], [], [], []),
		function(0,		% A0 input
			[1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31],
			[0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30],
			[], [], [], [], [])
		]) :-
	update_bounds(_, 21, _).


%  Iterate over all the TRUE vectors that need to be covered.
%  If no vectors remain to be covered (select_vector fails), then
%  the circuit is complete (printout results, update bounds, and
%  continue search for a lower cost circuit).
search(NumVars, NumGsIn, GsIn) :-
	select_vector(NumVars, NumGsIn, GsIn, Gj, Vector), !,
	cover_vector(NumVars, NumGsIn, GsIn, Gj, Vector, NumGs, Gs),
	add_necessary_functions(NumVars, NumGs, Gs, NumGsOut, GsOut),
	test_bounds(NumVars, NumGsOut, GsOut),
	search(NumVars, NumGsOut, GsOut).
search(NumVars, NumGs, Gs) :-
%	output_results(NumVars, NumGs, Gs),
	update_bounds(NumVars, NumGs, Gs),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Given the current solution, pick the best uncovered TRUE vector
%  for covering next.
%  The selected vector is specified by its vector number and function.
%  Select_vector fails if all TRUE vectors are covered.
%  Select_vector is determinant (gives only one solution).
select_vector(NumVars, NumGs, Gs, Gj, Vector) :-
	select_vector(Gs, NumVars, NumGs, Gs,
		dummy, 0, nf, 999, Gj, Vector, Type, _), !,
	\+ Type = cov,
	\+ Type = nf.

% loop over functions
select_vector([Gk|_], NumVars, _, _, Gj, V, Type, N, Gj, V, Type, N) :-
	function_number(Gk, K),
	K < NumVars.
select_vector([Gk|Gks], NumVars, NumGs, Gs,
		GjIn, Vin, TypeIn, Nin, GjOut, Vout, TypeOut, Nout) :-
	function_number(Gk, K),
	K >= NumVars,
	true_set(Gk, Tk),
	select_vector(Tk, Gk, NumVars, NumGs, Gs,
		GjIn, Vin, TypeIn, Nin, Gj, V, Type, N),
	select_vector(Gks, NumVars, NumGs, Gs,
		Gj, V, Type, N, GjOut, Vout, TypeOut, Nout).
	
% loop over vectors
select_vector([], _, _, _, _, Gj, V, Type, N, Gj, V, Type, N).
select_vector([V|Vs], Gk, NumVars, NumGs, Gs,
		GjIn, Vin, TypeIn, Nin, GjOut, Vout, TypeOut, Nout) :-
	vector_cover_type(NumVars, Gs, Gk, V, Type, N),
	best_vector(GjIn, Vin, TypeIn, Nin,
			Gk, V, Type, N,
			Gj2, V2, Type2, N2),
	select_vector(Vs, Gk, NumVars, NumGs, Gs,
		Gj2, V2, Type2, N2, GjOut, Vout, TypeOut, Nout).

vector_cover_type(NumVars, Gs, Gj, Vector, Type, NumCovers) :-
	immediate_predecessors(Gj, IPs),
	conceivable_inputs(Gj, CIs),
	false_set(Gj, Fj),
	cover_type1(IPs, Gs, Vector, nf, 0, T, N),
	cover_type2(CIs, Gs, NumVars, Fj, Vector, T, N, Type, NumCovers).

cover_type1([], _, _, T, N, T, N).
cover_type1([I|IPs], Gs, V, TypeIn, Nin, TypeOut, Nout) :-
	function(I, Gs, Gi),
	true_set(Gi, Ti),
	\+ set_member(V, Ti), !,
	false_set(Gi, Fi),
	(set_member(V, Fi) ->
		max_type(TypeIn, cov, Type);
		max_type(TypeIn, exp, Type)),
	N is Nin + 1,
	cover_type1(IPs, Gs, V, Type, N, TypeOut, Nout).
cover_type1([_|IPs], Gs, V, TypeIn, Nin, TypeOut, Nout) :-
	cover_type1(IPs, Gs, V, TypeIn, Nin, TypeOut, Nout).

cover_type2([], _, _, _, _, T, N, T, N).
cover_type2([I|CIs], Gs, NumVars, Fj, V, TypeIn, Nin, TypeOut, Nout) :-
	I < NumVars,
	function(I, Gs, Gi),
	false_set(Gi, Fi),
	set_member(V, Fi), !,
	max_type(TypeIn, var, Type),
	N is Nin + 1,
	cover_type2(CIs, Gs, NumVars, Fj, V, Type, N, TypeOut, Nout).
cover_type2([I|CIs], Gs, NumVars, Fj, V, TypeIn, Nin, TypeOut, Nout) :-
	I >= NumVars,
	function(I, Gs, Gi),
	true_set(Gi, Ti),
	\+ set_member(V, Ti), !,
	false_set(Gi, Fi),
	(set_member(V, Fi) ->
		(set_subset(Fj, Ti) ->
			max_type(TypeIn, fcn, Type);
			max_type(TypeIn, mcf, Type));
		(set_subset(Fj, Ti) ->
			max_type(TypeIn, exf, Type);
			max_type(TypeIn, exmcf, Type))),
	N is Nin + 1,
	cover_type2(CIs, Gs, NumVars, Fj, V, Type, N, TypeOut, Nout).
cover_type2([_|CIs], Gs, NumVars, Fj, V, TypeIn, Nin, TypeOut, Nout) :-
	cover_type2(CIs, Gs, NumVars, Fj, V, TypeIn, Nin, TypeOut, Nout).

%  The best vector to cover is the one with worst type, or, if types
%  are equal, with the least number of possible covers.
best_vector(dummy, _, _, _, Gj2, V2, Type2, N2, Gj2, V2, Type2, N2) :- !.
best_vector(Gj1, V1, Type1, N1, dummy, _, _, _, Gj1, V1, Type1, N1) :- !.
best_vector(Gj1, V1, Type, N1, Gj2, _, Type, N2, Gj1, V1, Type, N1) :-
	function_number(Gj1, J), function_number(Gj2, J),
	N1 < N2, !.
best_vector(Gj1, _, Type, N1, Gj2, V2, Type, N2, Gj2, V2, Type, N2) :-
	function_number(Gj1, J), function_number(Gj2, J),
	N1 >= N2, !.
best_vector(Gj1, V1, Type, N1, Gj2, _, Type, _, Gj1, V1, Type, N1) :-
	(Type = exp ; Type = var),
	function_number(Gj1, J1), function_number(Gj2, J2),
	J1 > J2, !.
best_vector(Gj1, _, Type, _, Gj2, V2, Type, N2, Gj2, V2, Type, N2) :-
	(Type = exp ; Type = var),
	function_number(Gj1, J1), function_number(Gj2, J2),
	J1 < J2, !.
best_vector(Gj1, V1, Type, N1, Gj2, _, Type, _, Gj1, V1, Type, N1) :-
	\+ (Type = exp ; Type = var),
	function_number(Gj1, J1), function_number(Gj2, J2),
	J1 < J2, !.
best_vector(Gj1, _, Type, _, Gj2, V2, Type, N2, Gj2, V2, Type, N2) :-
	\+ (Type = exp ; Type = var),
	function_number(Gj1, J1), function_number(Gj2, J2),
	J1 > J2, !.
best_vector(Gj1, V1, Type1, N1, _, _, Type2, _, Gj1, V1, Type1, N1) :-
	type_order(Type2, Type1), !.
best_vector(_, _, Type1, _, Gj2, V2, Type2, N2, Gj2, V2, Type2, N2) :-
	type_order(Type1, Type2), !.

max_type(T1, T2, T1) :- type_order(T1, T2), !.
max_type(T1, T2, T2) :- \+ type_order(T1, T2), !.

%  Order of types

type_order(cov, exp).
type_order(cov, var).
type_order(cov, fcn).
type_order(cov, mcf).
type_order(cov, exf).
type_order(cov, exmcf).
type_order(cov, nf).
type_order(exp, var).
type_order(exp, fcn).
type_order(exp, mcf).
type_order(exp, exf).
type_order(exp, exmcf).
type_order(exp, nf).
type_order(var, fcn).
type_order(var, mcf).
type_order(var, exf).
type_order(var, exmcf).
type_order(var, nf).
type_order(fcn, mcf).
type_order(fcn, exf).
type_order(fcn, exmcf).
type_order(fcn, nf).
type_order(mcf, exf).
type_order(mcf, exmcf).
type_order(mcf, nf).
type_order(exf, exmcf).
type_order(exf, nf).
type_order(exmcf, nf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Cover_vector will cover the specified vector and
%  generate new circuit information.
%  Using backtracking, all possible coverings are generated.
%  The ordering of the possible coverings is approximately that
%  given in Davidson's paper, but has been simplified.

cover_vector(NumVars, NumGsIn, GsIn, Gj, Vector, NumGsOut, GsOut) :-
	immediate_predecessors(Gj, IPs),
	conceivable_inputs(Gj, CIs),
	vector_types(Type),
	cover_vector(Type, IPs, CIs, Gj, Vector, NumVars, NumGsIn, GsIn,
		NumGsOut, GsOut).
	
vector_types(var).
vector_types(exp).
vector_types(fcn).
vector_types(mcf).
vector_types(exf).
vector_types(exmcf).
vector_types(nf).

cover_vector(exp, [I|_], _, Gj, V, _, NumGs, GsIn, NumGs, GsOut) :-
	function(I, GsIn, Gi),
	true_set(Gi, Ti),
	\+ set_member(V, Ti),
	update_circuit(GsIn, Gi, Gj, V, GsIn, GsOut).
cover_vector(exp, [_|IPs], _, Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	cover_vector(exp, IPs, _, Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut).
cover_vector(var, _, [I|_], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	I < NumVars,
	function(I, GsIn, Gi),
	false_set(Gi, Fi),
	set_member(V, Fi),
	update_circuit(GsIn, Gi, Gj, V, GsIn, GsOut).
cover_vector(var, _, [_|CIs], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	cover_vector(var, _, CIs, Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut).
cover_vector(fcn, _, [I|_], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	I >= NumVars,
	function(I, GsIn, Gi),
	false_set(Gi, Fi),
	set_member(V, Fi),
	true_set(Gi, Ti),
	false_set(Gj, Fj),
	set_subset(Fj, Ti),
	update_circuit(GsIn, Gi, Gj, V, GsIn, GsOut).
cover_vector(fcn, _, [_|CIs], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	cover_vector(fcn, _, CIs, Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut).
cover_vector(mcf, _, [I|_], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	I >= NumVars,
	function(I, GsIn, Gi),
	false_set(Gi, Fi),
	set_member(V, Fi),
	true_set(Gi, Ti),
	false_set(Gj, Fj),
	\+ set_subset(Fj, Ti),
	update_circuit(GsIn, Gi, Gj, V, GsIn, GsOut).
cover_vector(mcf, _, [_|CIs], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	cover_vector(mcf, _, CIs, Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut).
cover_vector(exf, _, [I|_], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	I >= NumVars,
	function(I, GsIn, Gi),
	false_set(Gi, Fi),
	\+ set_member(V, Fi),
	true_set(Gi, Ti),
	\+ set_member(V, Ti),
	false_set(Gj, Fj),
	set_subset(Fj, Ti),
	update_circuit(GsIn, Gi, Gj, V, GsIn, GsOut).
cover_vector(exf, _, [_|CIs], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	cover_vector(exf, _, CIs, Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut).
cover_vector(exmcf, _, [I|_], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	I >= NumVars,
	function(I, GsIn, Gi),
	false_set(Gi, Fi),
	\+ set_member(V, Fi),
	true_set(Gi, Ti),
	\+ set_member(V, Ti),
	false_set(Gj, Fj),
	\+ set_subset(Fj, Ti),
	update_circuit(GsIn, Gi, Gj, V, GsIn, GsOut).
cover_vector(exmcf, _, [_|CIs], Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut) :-
	cover_vector(exmcf, _, CIs, Gj, V, NumVars, NumGs, GsIn, NumGs, GsOut).
cover_vector(nf, _, _, Gj, V, NumVars, NumGsIn, GsIn, NumGsOut, GsOut) :-
	NumGsOut is NumGsIn + 1,
	false_set(Gj, Fj),
	new_function_CIs(GsIn,
		function(NumGsIn,Fj,[V],[],[],[],[],[]),
		NumVars, Gs, Gi),
	update_circuit(Gs, Gi, Gj, V, Gs, GsOut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_circuit([], _, _, _, _, []).
update_circuit([function(K,Tk,Fk,CIk,IPk,ISk,Pk,Sk)|GsIn],
		Gi, Gj, V, Gs,
		[function(K,Tko,Fko,CIko,IPko,ISko,Pko,Sko)|GsOut]) :-
	Gi = function(I,_,Fi,_,IPi,ISi,Pi,_),
	Gj = function(J,_,Fj,_,_,_,_,Sj),
	set_union([I], Pi, PiI),
	set_union([J], Sj, SjJ),
	(K = J ->
		set_union(Tk, Fi, Tk2);
		Tk2 = Tk),
	(K = I ->
		set_union(Tk2, Fj, Tk3);
		Tk3 = Tk2),
	((set_member(K, IPi); set_member(K, ISi)) ->
		set_union(Tk3, [V], Tko);
		Tko = Tk3),
	(K = I ->
		set_union(Fk, [V], Fko);
		Fko = Fk),
	((set_member(K, Pi); K = I) ->
		set_difference(CIk, SjJ, CIk2);
		CIk2 = CIk),
	((set_member(I, CIk), set_member(V, Fk)) ->
		set_difference(CIk2, [I], CIk3);
		CIk3 = CIk2),
	(K = I ->
		exclude_if_vector_in_false_set(CIk3, Gs, V, CIk4);
		CIk4 = CIk3),
	(K = J ->
		set_difference(CIk4, [I], CIko);
		CIko = CIk4),
	(K = J ->
		set_union(IPk, [I], IPko);
		IPko = IPk),
	(K = I ->
		set_union(ISk, [J], ISko);
		ISko = ISk),
	(set_member(K, SjJ) ->
		set_union(Pk, PiI, Pko);
		Pko = Pk),
	(set_member(K, PiI) ->
		set_union(Sk, SjJ, Sko);
		Sko = Sk),
	update_circuit(GsIn, Gi, Gj, V, Gs, GsOut).

exclude_if_vector_in_false_set([], _, _, []).
exclude_if_vector_in_false_set([K|CIsIn], Gs, V, CIsOut) :-
	function(K, Gs, Gk),
	false_set(Gk, Fk),
	set_member(V, Fk), !,
	exclude_if_vector_in_false_set(CIsIn, Gs, V, CIsOut).
exclude_if_vector_in_false_set([K|CIsIn], Gs, V, [K|CIsOut]) :-
	exclude_if_vector_in_false_set(CIsIn, Gs, V, CIsOut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_necessary_functions(NumVars, NumGsIn, GsIn, NumGsOut, GsOut) :-
	add_necessary_functions(NumVars, NumVars, NumGsIn, GsIn,
					NumGsOut, GsOut).

add_necessary_functions(NumGs, _, NumGs, Gs, NumGs, Gs) :- !.
add_necessary_functions(K, NumVars, NumGsIn, GsIn, NumGsOut, GsOut) :-
	function(K, GsIn, Gk),
	function_type(NumVars, NumGsIn, GsIn, Gk, nf, V), !,
	false_set(Gk, Fk),
	new_function_CIs(GsIn,
		function(NumGsIn,Fk,[V],[],[],[],[],[]),
		NumVars, Gs, Gl),
	function(K, Gs, Gk1),
	update_circuit(Gs, Gl, Gk1, V, Gs, Gs1),
	NumGs1 is NumGsIn + 1,
	K1 is K + 1,
	add_necessary_functions(K1, NumVars, NumGs1, Gs1, NumGsOut, GsOut).
add_necessary_functions(K, NumVars, NumGsIn, GsIn, NumGsOut, GsOut) :-
	K1 is K + 1,
	add_necessary_functions(K1, NumVars, NumGsIn, GsIn, NumGsOut, GsOut).

new_function_CIs(GsIn, function(L,Tl,Fl,_,IPl,ISl,Pl,Sl), NumVars,
		[GlOut|GsOut], GlOut) :-
	new_function_CIs(GsIn, L, Fl, NumVars, GsOut, [], CIlo),
	GlOut = function(L,Tl,Fl,CIlo,IPl,ISl,Pl,Sl).
	
new_function_CIs([], _, _, _, [], CIl, CIl).
new_function_CIs([function(K,Tk,Fk,CIk,IPk,ISk,Pk,Sk)|GsIn], L, Fl, NumVars,
		[function(K,Tk,Fk,CIko,IPk,ISk,Pk,Sk)|GsOut], CIlIn, CIlOut) :-
	set_intersection(Fl, Fk, []), !,
	(K >= NumVars ->
		set_union(CIk, [L], CIko);
		CIko = CIk),
	new_function_CIs(GsIn, L, Fl, NumVars, GsOut, [K|CIlIn], CIlOut).
new_function_CIs([Gk|GsIn], L, Fl, NumVars, [Gk|GsOut], CIlIn, CIlOut) :-
	new_function_CIs(GsIn, L, Fl, NumVars, GsOut, CIlIn, CIlOut).

function_type(NumVars, NumGs, Gs, Gk, Type, Vector) :-
	true_set(Gk, Tk),
	select_vector(Tk, Gk, NumVars, NumGs, Gs,
		dummy, 0, nf, 999, _, Vector, Type, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Cost and constraint predicates:

% very simple bound for now

test_bounds(_, NumGs, _) :-
	access(bound, Bound),
	NumGs < Bound.

update_bounds(_, NumGs, _) :-
	set(bound, NumGs).

% set and access for systems that don't support them
set(N, A) :-
	(recorded(N, _, Ref) -> erase(Ref) ; true),
	recorda(N, A, _).

access(N, A) :-
	recorded(N, A, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Output predicates:

%  for now just dump everything

%output_results(NumVars, NumGs, Gs) :-
%	NumGates is NumGs - NumVars,
%	write(NumGates), write(' gates'), nl,
%	write_gates(Gs), nl,
%	write('searching for a better solution...'), nl, nl.

write_gates([]).
write_gates([Gi|Gs]) :-
	function_number(Gi, I),
	write('gate #'), write(I), write(' inputs:   '),
	immediate_predecessors(Gi, IPi),
	write(IPi), nl,
	write_gates(Gs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Retrieve the specified function from the function list.
%  function(FunctionNumber, FunctionList, Function).
function(I, [Gi|_], Gi) :- function_number(Gi, I), !.
function(I, [_|Gs], Gi) :- function(I, Gs, Gi).

function_number(        function(I,_,_,_,_,_,_,_), I).
true_set(               function(_,T,_,_,_,_,_,_), T).
false_set(              function(_,_,F,_,_,_,_,_), F).
conceivable_inputs(     function(_,_,_,CI,_,_,_,_), CI).
immediate_predecessors( function(_,_,_,_,IP,_,_,_), IP).
immediate_successors(   function(_,_,_,_,_,IS,_,_), IS).
predecessors(           function(_,_,_,_,_,_,P,_), P).
successors(             function(_,_,_,_,_,_,_,S), S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Set operations assume that the sets are represented by an ordered list
%  of integers.

set_union([],     [],     []).
set_union([],     [X|L2], [X|L2]).
set_union([X|L1], [],     [X|L1]).
set_union([X|L1], [X|L2], [X|L3]) :-        set_union(L1, L2,     L3).
set_union([X|L1], [Y|L2], [X|L3]) :- X < Y, set_union(L1, [Y|L2], L3).
set_union([X|L1], [Y|L2], [Y|L3]) :- X > Y, set_union([X|L1], L2, L3).

set_intersection([],     [],     []).
set_intersection([],     [_|_],  []).
set_intersection([_|_],  [],     []).
set_intersection([X|L1], [X|L2], [X|L3]) :-    set_intersection(L1, L2,     L3).
set_intersection([X|L1], [Y|L2], L3) :- X < Y, set_intersection(L1, [Y|L2], L3).
set_intersection([X|L1], [Y|L2], L3) :- X > Y, set_intersection([X|L1], L2, L3).

set_difference([],     [],     []).
set_difference([],     [_|_],  []).
set_difference([X|L1], [],     [X|L1]).
set_difference([X|L1], [X|L2], L3) :-            set_difference(L1, L2,     L3).
set_difference([X|L1], [Y|L2], [X|L3]) :- X < Y, set_difference(L1, [Y|L2], L3).
set_difference([X|L1], [Y|L2], L3) :-     X > Y, set_difference([X|L1], L2, L3).

set_subset([],     _).
set_subset([X|L1], [X|L2]) :-        set_subset(L1, L2).
set_subset([X|L1], [Y|L2]) :- X > Y, set_subset([X|L1], L2).

set_member(X, [X|_]).
set_member(X, [Y|T]) :- X > Y, set_member(X, T).
