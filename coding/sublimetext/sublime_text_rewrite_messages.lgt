
:- category(sublime_text_rewrite_messages).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2016-10-04,
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

	message_hook([Issue| Tokens], Type) :-
		find_file_line(Tokens, File, Line),
		logtalk::message_prefix_stream(Type, core, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, ['~w:~d:1: '-[File, Line], Issue, nl]).

	find_file_line(['  in file ~w between lines ~w'-[File,Line-_]| _], File, Line) :-
		!.
	find_file_line(['  in file ~w at or above line ~d'-[File,Line]| _], File, Line) :-
		!.
	find_file_line([_| Tokens], File, Line) :-
		find_file_line(Tokens, File, Line).

:- end_category.
