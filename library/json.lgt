
:- set_prolog_flag(double_quotes, codes).


:- object(json).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2019-02-20,
		comment is 'JSON encoding and decoding predicates.'
	]).

	:- public(encode/2).
	:- mode(encode(+nonvar, -list(character_code)), one_or_error).
	:- info(encode/2, [
		comment is 'Converts a structure representing a JSON object into a string (list of character codes).',
		argnames is ['Structure', 'String']
	]).

	:- public(decode/2).
	:- mode(decode(+list(character_code), -nonvar), one_or_error).
	:- info(decode/2, [
		comment is 'Converts a string (list of character codes) into a structure representing a JSON object.',
		argnames is ['String', 'Structure']
	]).

	:- uses(list, [append/3]).
	:- uses(user, [format/3]).

	encode({}, [0'{, 0'}]).
	encode({Pairs}, [0'{| Encoding]) :-
		encode_pairs(Pairs, Encoding, [0'}]).
	encode(json(Pairs), [0'{| Encoding]) :-
		encode_pairs(Pairs, Encoding, [0'}]).
	encode(object(Pairs), [0'{| Encoding]) :-
		encode_pairs(Pairs, Encoding, [0'}]).

	encode_pairs((Pair, Pairs), Encoding, Tail) :-
		encode_pairs(Pairs, Pair, Encoding, Tail).
	encode_pairs([Pair| Pairs], Encoding, Tail) :-
		encode_pairs(Pairs, Pair, Encoding, Tail).
	encode_pairs([], Encoding, Encoding).
	encode_pairs(Key-Value, Encoding, Tail) :-
		encode_pair(Key-Value, PairEncoding),
		append(PairEncoding, Tail, Encoding).
	encode_pairs(Key=Value, Encoding, Tail) :-
		encode_pairs(Key-Value, Encoding, Tail).
	encode_pairs(Key:Value, Encoding, Tail) :-
		encode_pairs(Key-Value, Encoding, Tail).

	encode_pairs((Next, Pairs), Pair, Encoding, Tail) :-
		encode_pair(Pair, PairEncoding),
		append(PairEncoding, [0',| PairEncodings], Encoding),
		encode_pairs(Pairs, Next, PairEncodings, Tail).
	encode_pairs([Next| Pairs], Pair, Encoding, Tail) :-
		encode_pair(Pair, PairEncoding),
		append(PairEncoding, [0',| PairEncodings], Encoding),
		encode_pairs(Pairs, Next, PairEncodings, Tail).
	encode_pairs([], Pair, Encoding, Tail) :-
		encode_pair(Pair, PairEncoding),
		append(PairEncoding, Tail, Encoding).
	encode_pairs(Key:Value, Pair, Encoding, Tail) :-
		encode_pairs([Key:Value], Pair, Encoding, Tail).
	encode_pairs(Key-Value, Pair, Encoding, Tail) :-
		encode_pairs([Key-Value], Pair, Encoding, Tail).

	encode_pair(Key-Value, Encoding) :-
		encode_key(Key, KeyEncoding),
		encode_element(Value, ElementEncoding),
		append(KeyEncoding, [0':| ElementEncoding], Encoding).
	encode_pair(Key=Value, Encoding) :-
		encode_pair(Key-Value, Encoding).
	encode_pair(Key:Value, Encoding) :-
		encode_pair(Key-Value, Encoding).

	encode_key(Key, Encoding) :-
		encode_atom(Key, Encoding).

	encode_element({}, [0'{, 0'}]) :-
		!.
	encode_element({Pairs}, [0'{| Encoding]) :-
		!,
		encode_pairs(Pairs, Encoding, [0'}]).
	encode_element(json(Pairs), [0'{| Encoding]) :-
		!,
		encode_pairs(Pairs, Encoding, [0'}]).
	encode_element(object(Pairs), [0'{| Encoding]) :-
		!,
		encode_pairs(Pairs, Encoding, [0'}]).
	encode_element([], [0'[, 0']]) :-
		!.
	encode_element([Element| Elements], Encoding) :-
		encode_array(Elements, Element, ArrayEncoding),
		append([0'[| ArrayEncoding], [0']], Encoding).
	encode_element(Value, Encoding) :-
		atom(Value),
		!,
		encode_atom(Value, Encoding).
	encode_element(Value, Encoding) :-
		integer(Value),
		!,
		number_codes(Value, Encoding0),
		append(Encoding0, [0'., 0'0], Encoding).
	encode_element(Value, Encoding) :-
		float(Value),
		!,
		number_codes(Value, Encoding).

	encode_array([], Element, Encoding) :-
		encode_element(Element, Encoding).
	encode_array([Next| Elements], Element, Encoding) :-
		encode_element(Element, ElementEncoding),
		append(ElementEncoding, [0',| ElementsEncoding], Encoding),
		encode_array(Elements, Next, ElementsEncoding).

	encode_atom(Atom, Encoding) :-
		atom_codes(Atom, Codes),
		escape_codes(Codes, EscapedCodes),
		append([0'"| EscapedCodes], [0'"], Encoding).

	escape_codes([], []).
	escape_codes([Code| Codes], [0'\\, EscapedCode| EscapedCodes]) :-
		escaped_code(Code, EscapedCode),
		!,
		escape_codes(Code, Codes, EscapedCodes).
	escape_codes([Code| Codes], [Code| EscapedCodes]) :-
		escape_codes(Codes, EscapedCodes).

	escaped_code(34, 0'").
	escaped_code(92, 0'\\).
	escaped_code(34, 0'/).
	escaped_code( 8, 0'b).
	escaped_code( 9, 0't).
	escaped_code(10, 0'n).
	escaped_code(12, 0'f).
	escaped_code(13, 0'r).

	decode(String, JSON) :-
		phrase(json(JSON), String).

	json(json(Pairs)) -->
		json_white_space, "{", json_pairs(Pairs), "}", json_white_space.

	json_pairs([Pair| Pairs]) -->
		json_white_space,
		json_pair(Pair),
		",",
		json_white_space,
		json_pairs(Pairs).

	json_pair(Key-Value) -->
		json_string(Key),
		json_white_space,
		":",
		json_white_space,
		json_value(Value).

	json_array(Elements) -->
		json_white_space, "[", json_elements(Elements), "]", json_white_space.


	json_white_space -->
		[Code], {Code =< 32; Code >= 127},
		json_white_space.
	json_white_space -->
		[].


json_elements(Acc, Elements) -->
	json_skipws,
	(
	 json_value(V), json_skipws, ",", json_skipws,
	 !,
	 json_elements([V | Acc], Elements)
	)
	;
	(
	 (
	  json_value(V)
	  ->
	   {
	    All = [V | Acc],
	    reverse(All, Elements)
	   }
	 ;
	  %% Dangling comma, take what we have.
	  {reverse(Acc, Elements)}
	 )
	).


%%--------------------------------------------------------------------
%% json_members//2.
%%
%% Extraction of "members" of an object. A member is a sequence of one
%% or more comma separated "pairs". The order of extraction is
%% maintained in case it is important at a higher application level.
%%--------------------------------------------------------------------
json_members(Acc, Members) -->
	json_skipws,
	(
	 json_pair(P), json_skipws, ",", json_skipws,
	 !,
	 json_members([P | Acc], Members)
	)
	;
	(
	 (
	  json_pair(P)
	 ->
	  {
	   All = [P | Acc],
	   reverse(All, Members)
	  }
	 ;
	  {reverse(Acc, Members)}
	 )
	).


%%--------------------------------------------------------------------
%% json_value//1.
%%
%% Extraction of high-level objects for structure building. The cut is
%% to commit to the parsed term. Take it out, watch it go slower. Much
%% slower! Like, an order of magnitude slower. I think this is the
%% first time I appreciate what a well placed cut can actually do in
%% terms of performance.
%%--------------------------------------------------------------------
json_value(V) -->
	json_skipws,
	(
	 json_string(V), !
	;
	 json_number(V), !
	;
	 "true",  {V = true}, !
	;
	 "false", {V = false}, !
	;
	 "null",  {V = null}, !
	;
	 json_object(V), !
	;
	 json_array(V), !
	).




string(String) -->
	(
	 [34], [34], {Str = str([])}, !
	)
	;
	(
	 [34], json_strget([], Tmp), [34], !,
	 {Str = str(Tmp)}
	).


%%--------------------------------------------------------------------
%% json_strget//2
%%
%% Extraction of a single character. We have three predicates, the
%% first manages the sequence (backslash, double-quote), the second
%% manages generic characters and the final predicate is when we have
%% consumed the contents of the string and need to reverse the
%% accumulator for presentation.
%%--------------------------------------------------------------------
json_strget(Acc, Out) -->
	[92, Chr],
	json_strget([Chr, 92 | Acc], Out).

json_strget(Acc, Out) -->
	json_char(Chr),
	json_strget([Chr | Acc], Out).

json_strget(Acc, Str) --> [],
	{reverse(Acc, Str)}, !.


%%--------------------------------------------------------------------
%% json_char//1.
%%
%% Within the context of our DCG ruleset, we allow any thing into a
%% string except the double quote character. The only exception we
%% recognise is the sequence '\"' used to escape a double quote within
%% the string. The json_strget//2 predicate handles that situation.
%% anything BUT a double-quote character is a valid character
%%--------------------------------------------------------------------
json_char(C) --> [C], {C \= 34}.


json_number(Number) -->
	json_skipws,
	json_int(N), json_frac(F), json_exp(E, Exp),
	{json_make_number("~d.~d~s~d", [N, F, E, Exp], Number)}, !.

json_number(Number) -->
	json_skipws,
	json_int(N), json_frac(F),
	{json_make_number("~d.~d",[N, F], Number)}, !.

json_number(Number) -->
	json_skipws,
	json_int(N), json_exp(E, Exp),
	{json_make_number("~d.~s~d",[N, E, Exp], Number)}, !.

json_number(Number) -->
	json_skipws, json_int(Number), !.




%%--------------------------------------------------------------------
%% json_int//1
%%
%% Extraction of integers only, positive and negative.
%%--------------------------------------------------------------------
json_int(Val) -->
	( %% positive digit, >  0
	 json_digit19(D1),
	 json_digsum(D1, Val), !
	)
	;
	( %% negative multiple digits
	 "-",
	 json_digit19(D1),
	 json_digsum(D1, Total),
	 {Val is -Total}, !
	)
	;
	( %% negative single digit
	 "-",
	 json_digit(D1),
	 {Val is -D1};
	 json_digit(Val), !
	).

%%--------------------------------------------------------------------
%% json_frac//1.
%% json_exp//2.
%%
%% Extraction of fractional part and exponent for floating point
%% numbers.
%%--------------------------------------------------------------------
json_frac(Frac)  --> ".", json_digits(Frac).
json_exp(E, Exp) --> json_e(E), json_digits(Exp).


%%--------------------------------------------------------------------
%% json_e//1.
%%
%% Consume an exponent introducer for positive and negative values.
%%--------------------------------------------------------------------
json_e("E+") --> "e+", !.
json_e("E+") --> "E+", !.
json_e("E-") --> "e-", !.
json_e("E-") --> "E-", !.
json_e("E+") --> "e",  !.
json_e("E+") --> "E",  !.


%%--------------------------------------------------------------------
%% json_digit//1.
%%
%% Scan a single digit 0-9 or 1-9
%%--------------------------------------------------------------------
json_digit(D) --> [Chr], { "0"=<Chr, Chr=<"9", D is Chr-"0"}.
json_digit19(D) --> json_digit(D), {D > 0}.


%%--------------------------------------------------------------------
%% json_digits//1.
%% Scan a a series of digits, returns actual value
%%--------------------------------------------------------------------
json_digits(Val) --> json_digit(D0), json_digsum(D0, Val).
json_digits(Val) --> json_digit(Val).


%%--------------------------------------------------------------------
%% json_digsum//2.
%%
%% Scan/consume digits returning decimal value to json_digits//1.
%%--------------------------------------------------------------------
json_digsum(Acc, N) -->
	json_digit(D),
	{Acc1 is Acc*10 + D},
	json_digsum(Acc1, N).

json_digsum(N, N) --> [], !.

:- end_object.
