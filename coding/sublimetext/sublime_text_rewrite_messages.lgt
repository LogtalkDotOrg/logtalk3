
:- category(sublime_text_rewrite_messages).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/10/04,
		comment is 'Rewrite compiler error and warnings messages for Sublime Text builds.'
	]).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_, error, core, Tokens) :-
		message_hook(Tokens, error).
	logtalk::message_hook(_, error(Class), core, Tokens) :-
		message_hook(Tokens, error(Class)).
	logtalk::message_hook(_, warning, core, Tokens) :-
		message_hook(Tokens, warning).
	logtalk::message_hook(_, warning(Class), core, Tokens) :-
		message_hook(Tokens, warning(Class)).

	message_hook(Tokens, Type) :-
		reverse_line_order(Tokens, TokensReversed),
		logtalk::message_prefix_stream(Type, core, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, TokensReversed).

	reverse_line_order(Tokens, TokensReversed) :-
		append(FirstLine, [nl| OtherLines], Tokens),
		find_last_line(OtherLines, LastLine),
		append(LastLine, [':1:'-[]], SecondLine),
		append(SecondLine, FirstLine, TokensReversed0),
		append(TokensReversed0, [nl], TokensReversed).

	find_last_line(OtherLines, LastLine) :-
		append(Line, [nl| RestLines], OtherLines),
		find_last_line(RestLines, Line, LastLine).

	find_last_line([], LastLine, LastLine).
	find_last_line(RestLines, _, LastLine) :-
		find_last_line(RestLines, LastLine).

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

:- end_category.
