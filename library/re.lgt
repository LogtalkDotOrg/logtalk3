
:- object(re).

	:- info([
		version is 1.0,
		author is 'Robert D. Cameron; adapted to Logtalk by Paulo Moura.',
		date is 2011/01/03,
		comment is 'Perl Style Regular Expressions.'
	]).

	re_metachar(0'\\).
	re_metachar(0'|).
	re_metachar(0'*).
	re_metachar(0'+).
	re_metachar(0'.).
	re_metachar(0'[).
	re_metachar(0'$).
	re_metachar(0'().
	re_metachar(0')).

	set_metachar(0'\\).
	set_metachar(0']).
	set_metachar(0'-).

	re(Z) --> basic_re(W), re_tail(W, Z).

	re_tail(W, Z) --> "|", basic_re(X), re_tail(union(W,X), Z).
	re_tail(W, W) --> {true}.

	basic_re(Z) --> simple_re(W), basic_re_tail(W, Z).

	basic_re_tail(W, Z) --> simple_re(X), basic_re_tail(conc(W,X), Z).
	basic_re_tail(W, W) --> {true}.

	simple_re(Z) --> elemental_re(W), simple_re_tail(W, Z).

	simple_re_tail(W, star(W)) --> "*".
	simple_re_tail(W, plus(W)) --> "+".
	simple_re_tail(W, W) --> {true}.

	elemental_re(any) --> ".".
	elemental_re(group(X)) --> "(", re(X), ")".
	elemental_re(eos) --> "$".
	elemental_re(char(C)) --> [C], {\+ re_metachar(C)}.
	elemental_re(char(C)) --> "\\", [C], {re_metachar(C)}.
	%  For sets, first try the negative set syntax.  If the "[^" recognition
	%  succeeds, use cut to make sure that any subsequent failure does not
	%  cause the positive set interpretation to be used.
	elemental_re(negSet(X)) --> "[^", !, set_items(X), "]".
	elemental_re(posSet(X)) --> "[", set_items(X), "]".

	set_items([Item1| MoreItems]) --> set_item(Item1), set_items(MoreItems).
	set_items([Item1]) --> set_item(Item1).
	set_item(char(C)) --> [C], {\+ set_metachar(C)}.
	set_item(char(C)) --> "\\", [C], {set_metachar(C)}.
	set_item(range(A,B)) --> set_item(char(A)), "-", set_item(char(B)).

	%
	% re_match_1(RE, S, Unmatched, Selected) is true if RE matches
	% a string Prefix such that S = [Prefix|Unmatched], and
	% Selected is the list of substrings of Prefix that matched
	% the parenthesized components of RE.

	re_match_1(union(RE1, _RE2), S, U, Selected) :- 
		re_match_1(RE1, S, U, Selected).
	re_match_1(union(_RE1, RE2), S, U, Selected) :- 
		re_match_1(RE2, S, U, Selected).
	re_match_1(conc(RE1, RE2), S, U, Selected) :- 
		re_match_1(RE1, S, U1, Sel1),
		re_match_1(RE2, U1, U, Sel2),
		append(Sel1, Sel2, Selected).
	% Try longest match first.
	re_match_1(star(RE), S, U, Selected) :-
		re_match_1(RE, S, U1, Sel1),
		re_match_1(star(RE), U1, U, Sel2),
		append(Sel1, Sel2, Selected).
	re_match_1(star(_RE), S, S, []).
	re_match_1(plus(RE), S, U, Selected) :-
		re_match_1(RE, S, U1, Sel1),
		re_match_1(star(RE), U1, U, Sel2),
		append(Sel1, Sel2, Selected).
	% Match a group and add it to the end of
	% list of selected items from the submatch.
	re_match_1(group(RE), S, U, Selected) :-
		re_match_1(RE, S, U, Sel1),
		append(P, U, S),
		append(Sel1, [P], Selected).

	re_match_1(any, [_C1|U], U, []).
	% Note that the following works for matching both regular
	% characters and metacharacters.  
	re_match_1(char(C), [C|U], U, []).

	re_match_1(eos, [], [], []).

	re_match_1(negSet(Set), [C|U], U, []) :-
		\+ char_set_member(C, Set).

	re_match_1(posSet(Set), [C|U], U, []) :-
		char_set_member(C, Set).

	char_set_member(C, [char(C) | _]).
	char_set_member(C, [range(C1, C2) | _]) :-
		C1 =< C,
		C =< C2.
	char_set_member(C, [_|T]) :- char_set_member(C, T).

	%
	%  tokenize(RE, Input, Output) is true if
	%    - RE is the string representation of a regular expression,
	%         with tokens identified by parenthesized subexpressions 
	%    - Input is an input string
	%    - Output is the list of tokens extracted by repeated application
	%      of RE to Input.
	%
	tokenize(RE, Input, Output) :-
		phrase(re(Parsed_RE), RE),
		tokenize2(Parsed_RE, Input, Output).

	tokenize2(_P_RE, [], []).
	tokenize2(P_RE, Input, Output) :-
		re_match_1(P_RE, Input, Unmatched, SelStrings),
		names(Tokens, SelStrings),
		tokenize2(P_RE, Unmatched, MoreTokens),
		append(Tokens, MoreTokens, Output).

	names([], []).
	names([Sym1|MoreSymbols], [Str1|MoreStrings]) :-
		name(Sym1, Str1), 
		names(MoreSymbols, MoreStrings).

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

:- end_object.
