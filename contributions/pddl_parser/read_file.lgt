%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  read_line
%%%  This is a modified version for parsing pddl files.
%%%  Read the input file character by character and parse it
%%%  into a list. Brackets, comma, period and question marks
%%%  are treated as separate words. White spaces separed 
%%%  words. 
%%%
%%%  Similar to read_sent in Pereira and Shieber, Prolog and
%%%        Natural Language Analysis, CSLI, 1987.
%%%
%%%  Examples:
%%%           :- read_line('input.txt', L).
%%%           input.txt> The sky was blue, after the rain.
%%%           L = [the, sky, was, blue, (','), after, the, rain, '.']
%%%
%%%           :- read_line('domain.pddl', L).
%%%           domain.pddl>
%%%           (define (domain BLOCKS)
%%%             (:requirements :strips :typing :action-costs)
%%%             (:types block)
%%%             (:predicates (on ?x - block ?y - block)
%%%           ...
%%%           L = ['(', define, '(', domain, blocks, ')', '(', :, requirements|...].
%

:- category(read_file).

	:- info([
		version is 1.0,
		author is 'Robert Sasak, Charles University in Prague. Adapted to Logtalk by Paulo Moura.',
		date is 2011/08/04,
		comment is 'Utility predicates for parsing a file as a list of atoms.']).

	:- public(read_file/2).
	:- mode(read_file(+atom, -list(atom)), one).
	:- info(read_file/2,
		[comment is 'Reads a file character by character, parsing it into a list of atoms.',
		 argnames is ['File', 'List']]).

	read_file(File, List) :-
		current_input(Current),
		open(File, read, Stream),
		set_input(Stream),
		read_line(List),
		close(Stream),
		set_input(Current).

	read_line(Words) :-
		get_code(C),
		read_rest(C, Words).
          
	/* Ends the input. */
	read_rest(-1, []) :- !.

	/* Spaces, tabs and newlines between words are ignored. */
	read_rest(C, Words) :-
		ignored_character(C), !,
		get_code(C1),
		read_rest(C1, Words).

	/* Brackets, comma, period, colon, dash, or question marks are treated as separed words */
	read_rest(C, [Char| Words]) :-
		separed_word(C),
		char_code(Char, C),
		!,
		get_code(C1),
		read_rest(C1, Words).

	/* Read comments to the end of line (;) */
	read_rest(59, Words) :-
		get_code(Next),
		!, 
		read_comment(Next, Last),
		read_rest(Last, Words).

	/* Otherwise get all of the next word. */
	read_rest(C, [Word| Words]) :-
		read_word(C, Chars, Next),
		(	catch(number_codes(Word, Chars), _, fail) ->
			true
		;	atom_codes(Word, Chars)
		),
		read_rest(Next, Words).

	ignored_character(32).
	ignored_character(10).
	ignored_character( 9).
	ignored_character(13).
	ignored_character(92).

	separed_word(40).
	separed_word(41).
	separed_word(44).
	separed_word(45).
	separed_word(46).
	separed_word(63).
	separed_word(58).

	read_word(C, [], C) :-
		separate_word_marker(C),
		!.

	/* Otherwise, get characters and convert to lower case. */
	read_word(C, [LC| Chars], Last) :-
		lower_case(C, LC),
		get_code(Next),
		read_word(Next, Chars, Last).

	/* Space, comma, newline, period, end-of-file or question mark separate words. */
	separate_word_marker(32).
	separate_word_marker(44).
	separate_word_marker(10).
	separate_word_marker( 9).
	separate_word_marker(13).
	separate_word_marker(46).
	separate_word_marker(63).
	separate_word_marker(40).
	separate_word_marker(41).
	separate_word_marker(58).
	separate_word_marker(-1).

	/* Convert to lower case if necessary. */
	lower_case(C, C) :- ( C <  65 ; C > 90 ) , !.
	lower_case(C, LC) :- LC is C + 32.

	/* Keep reading as long you dont find end-of-line or end-of-file */
	read_comment(10, 10) :- !.
	read_comment(-1, -1) :- !.
	read_comment(_, Last) :-
		get_code(Next),
		read_comment(Next, Last).

	/* for reference ... 
	newline(10).
	comma(44).
	space(32).
	period(46).
	question_mark(63).
	*/

:- end_category.
