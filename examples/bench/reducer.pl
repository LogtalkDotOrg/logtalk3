%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A Graph Reducer for T-Combinators:
% Reduces a T-combinator expression to a final answer.  Recognizes
% the combinators I,K,S,B,C,S',B',C', cond, apply, arithmetic, tests,
% basic list operations, and function definitions in the data base stored
% as facts of the form t_def(_func, _args, _expr).
% Written by Peter Van Roy

% Uses write/1, compare/3, functor/3, arg/3.
top :-
	try(fac(3), _ans1),
%	write(_ans1), nl,
	try(quick([3,1,2]), _ans2).
%	write(_ans2), nl.

try(_inpexpr, _anslist) :-
	listify(_inpexpr, _list),
	curry(_list, _curry),
	t_reduce(_curry, _ans),
	make_list(_ans, _anslist).

%%	SWI-Prolog V7 compatibility hacks.

end(X) :- atom(X), !.
end(X) :- X == [].

list_functor_name(Name) :-
	functor([_|_], Name, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Examples of applicative functions which can be compiled & executed.
% This test version compiles them just before each execution.

% Factorial function:
t_def(fac, [N], cond(N=0, 1, N*fac(N-1))).

% Quicksort:
t_def(quick, [_l], cond(_l=[], [],
		 cond(tl(_l)=[], _l,
		 quick2(split(hd(_l),tl(_l)))))).
t_def(quick2, [_l], append(quick(hd(_l)), quick(tl(_l)))).

t_def(split, [_e,_l], cond(_l=[], [[_e]|[]],
		    cond(hd(_l)=<_e, inserthead(hd(_l),split(_e,tl(_l))),
		    inserttail(hd(_l),split(_e,tl(_l)))))).
t_def(inserthead, [_e,_l], [[_e|hd(_l)]|tl(_l)]).
t_def(inserttail, [_e,_l], [hd(_l)|[_e|tl(_l)]]).

t_def(append, [_a,_b], cond(_a=[], _b, [hd(_a)|append(tl(_a),_b)])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Full reduction:
% A dot '.' is printed for each reduction step.

t_reduce(_expr, _ans) :-
	atomic(_expr), !,
	 _ans=_expr.
% The reduction of '$cons' must be here to avoid an infinite loop
t_reduce([_y,_x|LF], [_yr,_xr|LF]) :-
	list_functor_name(LF),
	t_reduce(_x, _xr),
	!,
	t_reduce(_y, _yr),
	!.
t_reduce(_expr, _ans) :-
	t_append(_next, _red, _form, _expr),
%	write('.'),
	t_redex(_form, _red),
	!,
	t_reduce(_next, _ans),
	!.

t_append(_link, _link, _l, _l).
t_append([_a|_l1], _link, _l2, [_a|_l3]) :- t_append(_l1, _link, _l2, _l3).

% One step of the reduction:

% Combinators:
t_redex([_x,_g,_f,_k|sp], [[_xr|_g],[_xr|_f]|_k]) :- t_reduce(_x, _xr).
t_redex([_x,_g,_f,_k|bp], [[_x|_g],_f|_k]).
t_redex([_x,_g,_f,_k|cp], [_g,[_x|_f]|_k]).
t_redex([_x,_g,_f|s], [[_xr|_g]|[_xr|_f]]) :- t_reduce(_x, _xr).
t_redex([_x,_g,_f|b], [[_x|_g]|_f]).
t_redex([_x,_g,_f|c], [_g,_x|_f]).
t_redex([_y,_x|k], _x).
t_redex([_x|i], _x).

% Conditional:
t_redex([_elsepart,_ifpart,_cond|cond], _ifpart) :-
	t_reduce(_cond, _bool), _bool=true, !.
	% Does NOT work if _bool is substituted in the call!
t_redex([_elsepart,_ifpart,_cond|cond], _elsepart).

% Apply:
t_redex([_f|apply], _fr) :-
	t_reduce(_f, _fr).

% List operations:
t_redex([_arg|hd], _x) :-
	list_functor_name(LF),
	t_reduce(_arg, [_y,_x|LF]).
t_redex([_arg|tl], _y) :-
	list_functor_name(LF),
	t_reduce(_arg, [_y,_x|LF]).

% Arithmetic:
t_redex([_y,_x|_op], _res) :-
	end(_op),
	member(_op, ['+', '-', '*', '//', 'mod']),
	t_reduce(_x, _xres),
	t_reduce(_y, _yres),
	number(_xres), number(_yres),
	eval(_op, _res, _xres, _yres).

% Tests:
t_redex([_y,_x|_test], _res) :-
	end(_test),
	member(_test, [<, >, =<, >=, =\=, =:=]),
	t_reduce(_x, _xres),
	t_reduce(_y, _yres),
	number(_xres), number(_yres),
	(relop(_test, _xres, _yres)
	-> _res=true
	;  _res=false
	), !.

% Equality:
t_redex([_y,_x|=], _res) :-
	t_reduce(_x, _xres),
	t_reduce(_y, _yres),
	(_xres=_yres -> _res=true; _res=false), !.

% Arithmetic functions:
t_redex([_x|_op], _res) :-
	end(_op),
	member(_op, ['-']),
	t_reduce(_x, _xres),
	number(_xres),
	eval1(_op, _t, _xres).

% Definitions:
% Assumes a fact t_def(_func,_def) in the database for every
% defined function.
t_redex(_in, _out) :-
	append(_par,_func,_in),
	end(_func),
	t_def(_func, _args, _expr),
	t(_args, _expr, _def),
	append(_par,_def,_out).

% Basic arithmetic and relational operators:

eval(  +, C, A, B) :- C is A + B.
eval(  -, C, A, B) :- C is A - B.
eval(  *, C, A, B) :- C is A * B.
eval( //, C, A, B) :- C is A // B.
eval(mod, C, A, B) :- C is A mod B.

eval1(-, C, A) :- C is -A.

relop(  <, A, B) :- A<B.
relop(  >, A, B) :- A>B.
relop( =<, A, B) :- A=<B.
relop( >=, A, B) :- A>=B.
relop(=\=, A, B) :- A=\=B.
relop(=:=, A, B) :- A=:=B.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Scheme T:
% A Translation Scheme for T-Combinators

% Translate an expression to combinator form
% by abstracting out all variables in _argvars:
t(_argvars, _expr, _trans) :-
	listify(_expr, _list),
	curry(_list, _curry),
	t_argvars(_argvars, _curry, _trans), !.

t_argvars([], _trans, _trans).
t_argvars([_x|_argvars], _in, _trans) :-
	t_argvars(_argvars, _in, _mid),
	t_vars(_mid, _vars), % calculate variables in each subexpression
	t_trans(_x, _mid, _vars, _trans). % main translation routine

% Curry the original expression:
% This converts an applicative expression of any number
% of arguments and any depth of nesting into an expression
% where all functions are curried, i.e. all function
% applications are to one argument and have the form
% [_arg|_func] where _func & _arg are also of that form.
% Input is a nested function application in list form.
% Currying makes t_trans faster.
curry(_a, _a) :- (var(_a); atomic(_a)), !.
curry([_func|_args], _cargs) :-
	currylist(_args, _cargs, _func).

% Transform [_a1, ..., _aN] to [_cN, ..., _c1|_link]-_link
currylist([], _link, _link) :- !.
currylist([_a|_args], _cargs, _link) :-
	curry(_a, _c),
	currylist(_args, _cargs, [_c|_link]).

% Calculate variables in each subexpression:
% To any expression a list of the form
% [_vexpr, _astr, _fstr] is matched.
% If the expression is a variable or an atom
% then this list only has the first element.
% _vexpr = List of all variables in the expression.
% _astr, _fstr = Similar structures for argument & function.
t_vars(_v, [[_v]]) :- var(_v), !.
t_vars(_a, [[]]) :- atomic(_a), !.
t_vars([_func], [[]]) :- atomic(_func), !.
t_vars([_arg|_func], [_g,[_g1|_af1],[_g2|_af2]]) :-
	t_vars(_arg, [_g1|_af1]),
	t_vars(_func, [_g2|_af2]),
	unionv(_g1, _g2, _g).

% The main translation routine:
% trans(_var, _curriedexpr, _varexpr, _result)
% The translation scheme T in the article is followed literally.
% A good example of Prolog as a specification language.
t_trans(_x, _a, _, [_a|k]) :- (atomic(_a); var(_a), _a\==_x), !.
t_trans(_x, _y, _, i) :- _x==_y, !.
t_trans(_x, _e, [_ve|_], [_e|k]) :- notinv(_x, _ve).
t_trans(_x, [_f|_e], [_vef,_sf,_se], _res) :-
	_sf=[_vf|_],
	_se=[_ve|_other],
	(end(_e); _other=[_,[_ve1|_]], _ve1\==[]),
	t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, _res).
t_trans(_x, [_g|[_f|_e]], [_vefg,_sg,_sef], _res) :-
	_sg=[_vg|_],
	_sef=[_vef,_sf,_se],
	_se=[_ve|_],
	_sf=[_vf|_],
	t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, _res).

% First complex rule of translation scheme T:
t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, _e) :-
	notinv(_x, _ve), _x==_f, !.
t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, [_resf,_e|b]) :-
	notinv(_x, _ve), inv(_x, _vf), _x\==_f, !,
	t_trans(_x, _f, _sf, _resf).
t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, [_f,_rese|c]) :-
	/* inv(_x, _ve), */
	notinv(_x, _vf), !,
	t_trans(_x, _e, _se, _rese).
t_rule1(_x, _e, _ve, _se, _f, _vf, _sf, [_resf,_rese|s]) :-
	/* inv(_x, _ve), inv(_x, _vf), */
	t_trans(_x, _e, _se, _rese),
	t_trans(_x, _f, _sf, _resf).

% Second complex rule of translation scheme T:
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_g,_e|c]) :-
	_x==_f, notinv(_x, _vg), !.
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_resg,_e|s]) :-
	_x==_f, /* inv(_x, _vg), */ !,
	t_trans(_x, _g, _sg, _resg).
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_g,_resf,_e|cp]) :-
	/* _x\==_f, */ inv(_x, _vf), notinv(_x, _vg), !,
	t_trans(_x, _f, _sf, _resf).
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_resg,_resf,_e|sp]) :-
	/* _x\==_f, */ inv(_x, _vf), /* inv(_x, _vg), */ !,
	t_trans(_x, _f, _sf, _resf),
	t_trans(_x, _g, _sg, _resg).
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_f|_e]) :-
	/* notinv(_x, _vf), */ _x==_g, !.
t_rule2(_x, _e, _f, _vf, _sf, _g, _vg, _sg, [_resg,_f,_e|bp]) :-
	/* notinv(_x, _vf), inv(_x, _vg), _x\==_g, */
	t_trans(_x, _g, _sg, _resg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% List utilities:

% Convert curried list into a regular list:
make_list(_a, _a) :- atomic(_a).
make_list([_b,_a|LF], [_a|_rb]) :-
	list_functor_name(LF),
	make_list(_b, _rb).

listify(_X, _X) :-
	(var(_X); atomic(_X)), !.
listify(_Expr, [_Op|_LArgs]) :-
	functor(_Expr, _Op, N),
	listify_list(1, N, _Expr, _LArgs).

listify_list(I, N, _, []) :- I>N, !.
listify_list(I, N, _Expr, [_LA|_LArgs]) :- I=<N, !,
	arg(I, _Expr, _A),
	listify(_A, _LA),
	I1 is I+1,
	listify_list(I1, N, _Expr, _LArgs).

member(X, [X|_]).
member(X, [_|L]) :- member(X, L).

append([], L, L).
append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set utilities:
% Implementation inspired by R. O'Keefe, Practical Prolog.
% Sets are represented as sorted lists without duplicates.
% Predicates with 'v' suffix work with sets containing uninstantiated vars.

% *** Intersection
intersectv([], _, []).
intersectv([A|S1], S2, S) :- intersectv_2(S2, A, S1, S).

intersectv_2([], _, _, []).
intersectv_2([B|S2], A, S1, S) :-
        compare(Order, A, B),
        intersectv_3(Order, A, S1, B, S2, S).

intersectv_3(<, _, S1, B, S2,     S) :- intersectv_2(S1, B, S2, S).
intersectv_3(=, A, S1, _, S2, [A|S]) :- intersectv(S1, S2, S).
intersectv_3(>, A, S1, _, S2,     S) :- intersectv_2(S2, A, S1, S).

intersectv_list([], []).
intersectv_list([InS|Sets], OutS) :- intersectv_list(Sets, InS, OutS).

intersectv_list([]) --> [].
intersectv_list([S|Sets]) --> intersectv(S), intersectv_list(Sets).

% *** Difference
diffv([], _, []).
diffv([A|S1], S2, S) :- diffv_2(S2, A, S1, S).

diffv_2([], A, S1, [A|S1]).
diffv_2([B|S2], A, S1, S) :-
        compare(Order, A, B),
        diffv_3(Order, A, S1, B, S2, S).

diffv_3(<, A, S1, B, S2, [A|S]) :- diffv(S1, [B|S2], S).
diffv_3(=, A, S1, _, S2,     S) :- diffv(S1, S2, S).
diffv_3(>, A, S1, _, S2,     S) :- diffv_2(S2, A, S1, S).

% *** Union
unionv([], S2, S2).
unionv([A|S1], S2, S) :- unionv_2(S2, A, S1, S).

unionv_2([], A, S1, [A|S1]).
unionv_2([B|S2], A, S1, S) :-
        compare(Order, A, B),
        unionv_3(Order, A, S1, B, S2, S).

unionv_3(<, A, S1, B, S2, [A|S]) :- unionv_2(S1, B, S2, S).
unionv_3(=, A, S1, _, S2, [A|S]) :- unionv(S1, S2, S).
unionv_3(>, A, S1, B, S2, [B|S]) :- unionv_2(S2, A, S1, S).

% *** Subset
subsetv([], _).
subsetv([A|S1], [B|S2]) :-
        compare(Order, A, B),
        subsetv_2(Order, A, S1, S2).

subsetv_2(=, _, S1, S2) :- subsetv(S1, S2).
subsetv_2(>, A, S1, S2) :- subsetv([A|S1], S2).

% For unordered lists S1:
small_subsetv([], _).
small_subsetv([A|S1], S2) :- inv(A, S2), small_subsetv(S1, S2).

% *** Membership
inv(A, [B|S]) :-
        compare(Order, A, B),
        inv_2(Order, A, S).

inv_2(=, _, _).
inv_2(>, A, S) :- inv(A, S).

% *** Non-membership
notinv(A, S) :- notinv_2(S, A).

notinv_2([], _).
notinv_2([B|S], A) :-
        compare(Order, A, B),
        notinv_3(Order, A, S).

notinv_3(<, _, _).
notinv_3(>, A, S) :- notinv_2(S, A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
