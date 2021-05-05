:- encoding(Encoding).	% this is a single-line comment

/*
this is
a block
comment
*/


:- if(Goal).
	% conditional
:- elif(Goal).
	% compilation
:- else.
	% directives
:- endif.


:- initialization(Goal).
:- op(Precedence, Associativity, Operator).
:- ensure_loaded(File).
:- include(File).
:- set_prolog_flag(Flag, Value).
:- set_logtalk_flag(Flag, Value).


:- module(module, [foo/1, bar/2]).

:- use_module(library).
:- use_module(library, [baz/3]).

:- reexport(library).
:- reexport(library, [qux/4]).

:- export(quux/5).

:- initialization(some_goal(X, Y)).

:- use_module(module).
:- use_module(module, [append/3, member/2]).

:- multifile(zzz/1).
:- multifile(module:zzz/1).

:- use_module(module, [xxx/1, yyy/2, zzz/3]).
:- export(bbb/3).
:- reexport(cccc/4).

:- meta_predicate(aaa(:, *)).
:- discontiguous(aaa/2).

:- dynamic(ccc/2).

reflection :-
	current_predicate(Predicate),
	predicate_property(Predicate, Property).

database :-
	abolish(Functor/Arity),
	asserta(Clause),
	assertz(Clause),
	clause(Head, Body),
	retract(Clause),
	retractall(Head).

exception :-
	catch(Goal, Error, Catcher),
	throw(Error).

all_solutions :-
	bagof(Term, Goal, List),
	bagof(Term, Var^Goal, List),
	findall(Term, Goal, List),
	forall(Generate, Test),
	setof(Term, Goal, List),
	setof(Term, Var1^Var2^Goal, List).

dcg_rules_parsing :-
	phrase(NonTerminal, Input, Rest).

term_expansion :-
	expand_term(Term, Expanded),
	expand_goal(Goal, Expanded),
	term_expansion(Term, Expanded),
	goal_expansion(Goal, Expanded).

explicitly_qualified_module_calls :-
	Module:Goal.

if_then_else :-
	(	If ->
		Then
	;	Else
	).

numbers :-
	X1 is 13, X2 is -13, X3 is +13,
	Y1 is 13.13, Y2 is -13.13, Y3 is +13.13,
	Z1 is 13.13e-23, Z2 is -13.13e-23, Z3 is +13.13e-23,
	C1 is 0'A, C2 is 0'', C3 is 0'",
	B1 is 0b1011101,
	O1 is 0o1234560,
	H1 is 0x1234567890abcDEF.

functions :-
	A is atan(3.14) + acos(0.5) + asin(0.5) + sin(0.77) - cos(123.23),
	B is sign(-12) * abs(35/78),
	C is truncate(3.14) + round(-7.8) - ceiling(111.88),
	D is exp(3.8) - log(123.98) / sqrt(33) * 23 ** 4,
	E is rem(3, 2) + mod(5, 3) * 2 rem 2 // 5 mod 3 + pi * e,
	F is float_fractional_part(3.14) + float_integer_part(3.14),
	G is float(33) + floor(99.99),
	I is min(3,4) + max(4,5).

bitwise :-
	A is 16 >> 2,
	B is 16 << 2,
	C is 10 /\ 12,
	D is 10 \/ 12,
	E is \ 10.

term_unification :-
	Term1 = Term2,
	Term1 \= Term2,
	unify_with_occurs_check(Term1, Term2),
	subsumes_term(General, Specific).

term_testing :-
	atom(Atom),
	atomic(Atomic),
	integer(Integer),
	float(Float),
	callable(Term),
	compound(Term),
	nonvar(Term),
	var(Term),
	number(Number),
	ground(Term),
	acyclic_term(Term).

term_comparison :-
	compare(Order, Term1, Term2),
	Term1 == Term2,
	Term1 \== Term2,
	Term1 @< Term2,
	Term1 @=< Term2,
	Term1 @>= Term2,
	Term1 @> Term2.

term_creation_and_decomposition :-
	functor(Term, Functor, Arity),
	arg(N, Term, Arg),
	Term =.. [Functor| Args],
	copy_term(Term, Copy),
	numbervars(Term, Start, End),
	term_variables(Term, Variables).

arithemtic_evaluation :-
	X is Expression.

arithmetic_comparison :-
	Exp1 =:= Exp2,
	Exp1 =\= Exp2,
	Exp1 < Exp2,
	Exp1 =< Exp2,
	Exp1 > Exp2,
	Exp1 >= Exp2.

stream_selection_and_control :-
	current_input(Stream),
	current_output(Stream),
	set_input(Stream),
	set_output(Stream),
	open(Source, Mode, Stream, Options),
	close(Stream),
	flush_output(Stream),
	stream_property(Stream, Property),
	at_end_of_stream(Stream),
	set_stream_position(Stream, Position),
	flush_output,
	at_end_of_stream.

character_input_output :-
	get_char(Char),
	get_code(Code),
	peek_char(Char),
	peek_code(Code),
	put_char(Char),
	put_code(Code),
	nl(Stream),
	nl.

byte_input_output :-
	get_byte(Byte),
	peek_byte(Byte),
	put_byte(Byte).

term_input_output :-
	read(Term),
	read_term(Stream, Term, Options),
	write(Term),
	write(Term),
	write_canonical(Term),
	write_term(Stream, Term, Options),
	current_op(Precedence, Associativity, Operator),
	op(Precedence, Associativity, Operator),
	current_char_conversion(InChar, OutChar),
	char_conversion(InChar, OutChar).

logic_and_control :-
	\+ Goal,
	call(Goal),
	once(Goal),
	ignore(Goal),
	true,
	fail,
	false,
	repeat,
	!.

atomic_term_processing :-
	atom_length(Atom, Length),
	atom_chars(Atom, Chars),
	atom_codes(Atom, Codes),
	atom_concat(Atom1, Atom2, Atom),
	sub_atom(Atom, Before, Length, After, SubAtom),
	char_code(Char, Code),
	number_chars(Number, Chars),
	number_codes(Number, Codes).

implementation_defined_hooks :-
	current_prolog_flag(Flag, Value),
	set_prolog_flag(Flag, Value),
	halt(ExitCode),
	halt.

sorting :-
	keysort(List, Sorted),
	sort(List, Sorted).

number(C) --> "+", number(C).
number(C) --> "-", number(X), {C is -X}.
number(X) --> [C], {0'0 =< C, C =< 0'9, X is C - 0'0}.

escape_sequences :-
	write('Quoted atom with a quote ('') inside.'),
	write('Quoted atom with a backslash (\\) inside.'),
	write('Quoted atom with control escape sequences: \a \b \r \f \t \n \v'),
	write('Quoted atom with an octal escape sequence: \123\.'),
	write('Quoted atom with an hexadecimal escape sequence: \x123f\.').
