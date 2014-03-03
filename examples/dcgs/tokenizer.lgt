%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Natural Language Processing in Prolog using Definite Clause Grammar rules
% 
% This example is a straightforward adaptation of the original plain Prolog 
% code described in the paper:
%
%	Tokenization using DCG Rules 
%	Michael A. Covington 
%	Artificial Intelligence Center 
%	The University of Georgia 
%	Athens, Georgia 30602-7415 U.S.A. 
%	2000 April 21
%
% A copy of the paper is available at:
%
%	http://www.ai.uga.edu/~mc/projpaper.ps
%
% Usage example:
%
%	| ?- tokenizer::tokens(" We owe $1,048,576.24 to Agent 007 for Version 3.14159! ", Tokens).

:- object(tokenizer).

	:- info([
		version is 1.0,
		date is 2006/2/11,
		author is 'Michael A. Covington',
		comment is 'Natural language tokenizer example using DCG rules.'
	]).

	:- public(tokens/2).
	:- mode(tokens(+string, -list), zero_or_more).
	:- info(tokens/2, [
		comment is 'Parses a string into a list of tokens.',
		argnames is ['String', 'Tokens']
	]).

	tokens(String, Tokens) :-
		phrase(token_list(Tokens), String).

	% A token list is a series of zero or more tokens.
	% Its argument consists of the list of tokens, as atoms and numbers.
	% The cut ensures that the maximum number of characters is gathered into each token.

	token_list([T| Rest]) --> blank0, token(T), !, token_list(Rest).
	token_list([]) --> blank0.

	% blank0 is a series of zero or more blanks.

	blank0 --> [C], {char_type(C, blank)}, !, blank0.
	blank0 --> [].

	% Several kinds of tokens.
	% This is where lists of characters get converted into atoms or numbers.

	token(T) --> special(L), {atom_codes(T, L)}.
	token(T) --> word(W), {atom_codes(T, W)}.
	token(T) --> numeral(N), {number_codes(T, N)}.

	% A word is a series of one or more letters.
	% The rules are ordered so that we first try to gather as many
	% characters into one digit_string as possible.

	word([L| Rest]) --> letter(L), word(Rest).
	word([L]) --> letter(L).

	% A numeral is a list of characters that constitute a number.
	% The argument of numeral(...) is the list of character codes.

	numeral([C1, C2, C3| N]) --> ",", digit(C1), digit(C2), digit(C3), numeral(N).
	numeral([C1, C2, C3]) --> ",", digit(C1), digit(C2), digit(C3).
	numeral([C| N]) --> digit(C), numeral(N).	% multiple digits
	numeral([C]) --> digit(C).					% single digit
	numeral(N) --> decimal_part(N).				% decimal point and more digits

	decimal_part([46| Rest]) --> ".", digit_string(Rest).

	digit_string([D| N]) --> digit(D), digit_string(N).
	digit_string([D]) --> digit(D).

	% Various kinds of characters...

	digit(C) --> [C], {char_type(C, numeric)}.

	special([C]) --> [C], {char_type(C, special)}.

	letter(C) --> [C], {char_type(C, lowercase)}.
	letter(C) --> [U], {char_type(U, uppercase), C is U + 32}.	% Conversion to lowercase

	% char_type(+Code, ?Type)
	% Classifies a character (ASCII code) as blank, numeric, uppercase, lowercase, or special.
	% Adapted from Covington 1994.

	char_type(Code, Type) :-	% blanks, other ctrl codes 
		Code =< 32,
		!,
		Type = blank.

	char_type(Code, Type) :-	% digits
		48 =< Code, Code =< 57,
		!,
		Type = numeric.

	char_type(Code, Type) :-	% lowercase letters
		97 =< Code, Code =< 122,
		!,
		Type = lowercase.

	char_type(Code, Type) :-	% uppercase letters
		65 =< Code, Code =< 90,
		!,
		Type = uppercase.

	char_type(_, special).		% all others

:- end_object.
