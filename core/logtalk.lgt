%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% the compiler/runtime must be able to call some of the code generated
% by the compilation of the `logtalk` object directly, thus forcing us
% to fix the code prefix that is used in its compilation
:- set_logtalk_flag(code_prefix, '$').


:- object(logtalk).

	:- info([
		version is 1:17:2,
		author is 'Paulo Moura',
		date is 2020-10-03,
		comment is 'Built-in object providing message printing, debugging, library, source file, and hacking methods.',
		remarks is [
			'Default message kinds' - '``silent``, ``silent(Key)``, ``banner``, ``help``, ``comment``, ``comment(Key)``, ``information``, ``information(Key)``, ``warning``, ``warning(Key)``, ``error``, ``error(Key)``, ``debug``, ``debug(Key)``, ``question``, and ``question(Key)``.',
			'Printing of silent messages' - 'By default, silent messages are not printed. These messages are only useful when intercepted.',
			'Printing of banner and comment messages' - 'By default, banner and comment messages are only printed when the ``report`` flag is turned on.',
			'Printing of help, information, and question messages' - 'These messages are always printed by default as they provide requested output.',
			'Printing of warning messages' - 'By default, warning messages are not printed when the ``report`` flag is turned off.',
			'Printing of error messages' - 'These messages are always printed by default.',
			'Printing of debug messages' - 'By default, debug messages are only printed when the ``debug`` flag is turned on. The compiler suppresses debug messages when compiling in optimized mode.',
			'Meta messages' - 'A meta message is a message that have another message as argument and is typically used for debugging messages. Meta messages avoid the need of defining tokenizer rules for every message but can be intercepted as any other message.',
			'@Message meta message' - 'By default, the message is printed as passed to the ``write/1`` predicate followed by a newline.',
			'Key-Value meta message' - 'By default, the message is printed as "Key: Value" followed by a newline. The key is printed as passed to the ``write/1`` predicate while the value is printed as passed to the ``writeq/1`` predicate.',
			'Format+Arguments meta message' - 'By default, the message is printed as passed to the ``format/2`` predicate.',
			'List meta message' - 'By default, the list items are printed indented one per line. The items are preceded by a dash and can be ``@Message``, ``Key-Value``, or ``Format+Arguments`` messages. If that is not the case, the item is printed as passed to the ``writeq/1`` predicate.',
			'Title::List meta message' - 'By default, the title is printed followed by a newline and the indented list items, one per line. The items are printed as in the ``List`` meta message.'
		]
	]).

	:- built_in.

	:- set_logtalk_flag(context_switching_calls, allow).
	:- set_logtalk_flag(dynamic_declarations, deny).
	:- set_logtalk_flag(complements, deny).
	:- set_logtalk_flag(events, deny).
	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	% message printing predicates

	:- public(print_message/3).
	:- mode(print_message(+nonvar, +nonvar, +nonvar), one).
	:- info(print_message/3, [
		comment is 'Prints a message of the given kind for the specified component.',
		argnames is ['Kind', 'Component', 'Message']
	]).

	:- public(print_message_tokens/3).
	:- mode(print_message_tokens(@stream_or_alias, +atom, @list(nonvar)), one).
	:- info(print_message_tokens/3, [
		comment is 'Print the messages tokens to the given stream, prefixing each line with the specified atom.',
		argnames is ['Stream', 'Prefix', 'Tokens']
	]).

	:- public(print_message_token/4).
	:- multifile(print_message_token/4).
	:- dynamic(print_message_token/4).
	:- mode(print_message_token(@stream_or_alias, @atom, @nonvar, @list(nonvar)), zero_or_one).
	:- info(print_message_token/4, [
		comment is 'User-defined hook predicate for printing a message token (``at_same_line``, ``nl``, ``flush``, ``Format-Arguments``, ``term(Term,Options)``, ``ansi(Attributes,Format,Arguments)``, ``begin(Kind,Variable)``, and ``end(Variable)``).',
		argnames is ['Stream', 'Prefix', 'Token', 'Tokens']
	]).

	:- public(message_tokens//2).
	:- multifile(message_tokens//2).
	:- dynamic(message_tokens//2).
	:- mode(message_tokens(+nonvar, +nonvar), zero_or_one).
	:- info(message_tokens//2, [
		comment is 'User-defined hook grammar rule for converting a message into a list of tokens (``at_same_line``, ``nl``, ``flush``, ``Format-Arguments``, ``term(Term,Options)``, ``ansi(Attributes,Format,Arguments)``, ``begin(Kind,Variable)``, and ``end(Variable)``).',
		argnames is ['Message', 'Component']
	]).

	:- public(message_prefix_stream/4).
	:- multifile(message_prefix_stream/4).
	:- dynamic(message_prefix_stream/4).
	:- mode(message_prefix_stream(?nonvar, ?nonvar, ?atom, ?stream_or_alias), zero_or_more).
	:- info(message_prefix_stream/4, [
		comment is 'Message line prefix and output stream to be used when printing a message given its kind and component.',
		argnames is ['Kind', 'Component', 'Prefix', 'Stream']
	]).

	:- public(message_hook/4).
	:- multifile(message_hook/4).
	:- dynamic(message_hook/4).
	:- mode(message_hook(+nonvar, +nonvar, +nonvar, +list(nonvar)), zero_or_one).
	:- info(message_hook/4, [
		comment is 'User-defined hook predicate for intercepting message printing calls.',
		argnames is ['Message', 'Kind', 'Component', 'Tokens']
	]).

	% question asking predicates

	:- public(ask_question/5).
	:- meta_predicate(ask_question(*, *, *, 1, *)).
	:- mode(ask_question(+nonvar, +nonvar, +nonvar, +callable, -term), one).
	:- info(ask_question/5, [
		comment is 'Asks a question and reads the answer until the check predicate is true.',
		argnames is ['Kind', 'Component', 'Question', 'Check', 'Answer']
	]).

	:- public(question_hook/6).
	:- multifile(question_hook/6).
	:- dynamic(question_hook/6).
	:- meta_predicate(question_hook(*, *, *, *, 1, *)).
	:- mode(question_hook(+nonvar, +nonvar, +nonvar, +list(nonvar), +callable, -term), zero_or_one).
	:- info(question_hook/6, [
		comment is 'User-defined hook predicate for intercepting question asking calls.',
		argnames is ['Question', 'Kind', 'Component', 'Tokens', 'Check', 'Answer']
	]).

	:- public(question_prompt_stream/4).
	:- multifile(question_prompt_stream/4).
	:- dynamic(question_prompt_stream/4).
	:- mode(question_prompt_stream(?nonvar, ?nonvar, ?atom, ?stream_or_alias), zero_or_more).
	:- info(question_prompt_stream/4, [
		comment is 'Prompt and input stream to be used when asking a question given its kind and component.',
		argnames is ['Kind', 'Component', 'Prompt', 'Stream']
	]).

	% debugging predicates

	:- public(trace_event/2).
	:- multifile(trace_event/2).
	:- dynamic(trace_event/2).
	:- mode(trace_event(@callable, @execution_context), zero).
	:- info(trace_event/2, [
		comment is 'Trace event handler. The runtime calls all trace event handlers using a failure-driven loop before calling the debug event handler.',
		argnames is ['Event', 'ExecutionContext']
	]).

	:- public(debug_handler_provider/1).
	:- multifile(debug_handler_provider/1).
	:- mode(debug_handler_provider(?object_identifier), zero_or_one).
	:- info(debug_handler_provider/1, [
		comment is 'Declares an object as the debug handler provider. There should be at most one debug handler provider loaded at any given moment.',
		argnames is ['Provider']
	]).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(debug_handler_provider/1).
	:- endif.

	:- public(debug_handler/2).
	:- multifile(debug_handler/2).
	:- mode(debug_handler(?entity_identifier, ?atom), zero_or_more).
	:- info(debug_handler/2, [
		comment is 'Debug event handler. The defined events are unification events - ``fact(Entity,Fact,Clause,File,Line)`` and ``rule(Entity,Head,Clause,File,Line)`` - and goal events - ``top_goal(Goal,CompiledGoal)`` and ``goal(Goal,CompiledGoal)``.',
		argnames is ['Event', 'ExecutionContext']
	]).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(debug_handler/2).
	:- endif.

	% file and library predicates

	:- public(expand_library_path/2).
	:- mode(expand_library_path(+atom, ?atom), zero_or_one).
	:- mode(expand_library_path(+callable, ?atom), zero_or_one).
	:- info(expand_library_path/2, [
		comment is 'Expands a library alias or a library path into its absolute path. Uses a depth bound to prevent loops.',
		argnames is ['LibraryPath', 'AbsolutePath']
	]).

	:- public(loaded_file/1).
	:- mode(loaded_file(?atom), zero_or_more).
	:- info(loaded_file/1, [
		comment is 'Enumerates, by backtracking, all loaded files, returning their full paths.',
		argnames is ['Path']
	]).

	:- public(loaded_file_property/2).
	:- mode(loaded_file_property(?atom, ?compound), zero_or_more).
	:- info(loaded_file_property/2, [
		comment is 'Enumerates, by backtracking, loaded file properties: basename/1, directory/1, mode/1, flags/1, text_properties/1 (encoding/1 and bom/1), target/1, modified/1, parent/1, includes/1, library/1, object/1, protocol/1, and category/1.',
		argnames is ['Path', 'Property']
	]).

	:- public(file_type_extension/2).
	:- mode(file_type_extension(?atom, ?atom), zero_or_more).
	:- info(file_type_extension/2, [
		comment is 'Enumerates, by backtracking, all defined file type extensions. The defined types are: ``source``, ``object``, ``logtalk``, ``prolog``, and ``tmp``. The source type returns both ``logtalk`` and ``prolog`` type extensions.',
		argnames is ['Type', 'Extension']
	]).

	% hacking predicates

	:- public(compile_aux_clauses/1).
	:- mode(compile_aux_clauses(@list(clause)), one).
	:- info(compile_aux_clauses/1, [
		comment is 'Compiles a list of auxiliary clauses. Can only be called during source file compilation, usually from ``term_expansion/2`` or ``goal_expansion/2`` hook predicate definitions.',
		argnames is ['Clauses']
	]).

	:- public(entity_prefix/2).
	:- mode(entity_prefix(?entity_identifier, ?atom), zero_or_one).
	:- info(entity_prefix/2, [
		comment is 'Converts between an entity identifier and the entity prefix that is used for its compiled code. When none of the arguments is instantiated, it returns the identifier and the prefix of the entity under compilation, if any.',
		argnames is ['Entity', 'Prefix']
	]).

	:- public(compile_predicate_heads/4).
	:- mode(compile_predicate_heads(@list(callable), ?entity_identifier, -list(callable), @execution_context), zero_or_one).
	:- mode(compile_predicate_heads(@conjunction(callable), ?entity_identifier, -conjunction(callable), @execution_context), zero_or_one).
	:- mode(compile_predicate_heads(@callable, ?entity_identifier, -callable, @execution_context), zero_or_one).
	:- info(compile_predicate_heads/4, [
		comment is 'Compiles clause heads. The heads are compiled in the context of the entity under compilation when the entity argument is not instantiated.',
		argnames is ['Heads', 'Entity', 'CompiledHeads', 'ExecutionContext']
	]).

	:- public(compile_predicate_indicators/3).
	:- mode(compile_predicate_indicators(@list(predicate_indicator), ?entity_identifier, -list(predicate_indicator)), zero_or_one).
	:- mode(compile_predicate_indicators(@conjunction(predicate_indicator), ?entity_identifier, -conjunction(predicate_indicator)), zero_or_one).
	:- mode(compile_predicate_indicators(@predicate_indicator, ?entity_identifier, -predicate_indicator), zero_or_one).
	:- info(compile_predicate_indicators/3, [
		comment is 'Compiles predicate indicators. The predicate are compiled in the context of the entity under compilation when the entity argument is not instantiated.',
		argnames is ['PredicateIndicators', 'Entity', 'CompiledPredicateIndicators']
	]).

	:- public(decompile_predicate_heads/4).
	:- mode(decompile_predicate_heads(@list(callable), -entity_identifier, -atom, -list(callable)), zero_or_one).
	:- mode(decompile_predicate_heads(@conjunction(callable), -entity_identifier, -atom, -conjunction(callable)), zero_or_one).
	:- mode(decompile_predicate_heads(@callable, -entity_identifier, -atom, -callable), zero_or_one).
	:- info(decompile_predicate_heads/4, [
		comment is 'Decompiles clause heads. All compiled clause heads must belong to the same entity, which must be loaded.',
		argnames is ['CompiledHeads', 'Entity', 'Type', 'Heads']
	]).

	:- public(decompile_predicate_indicators/4).
	:- mode(decompile_predicate_indicators(@list(predicate_indicator), -entity_identifier, -atom, -list(predicate_indicator)), zero_or_one).
	:- mode(decompile_predicate_indicators(@conjunction(predicate_indicator), -entity_identifier, -atom, -conjunction(predicate_indicator)), zero_or_one).
	:- mode(decompile_predicate_indicators(@predicate_indicator, -entity_identifier, -atom, -predicate_indicator), zero_or_one).
	:- info(decompile_predicate_indicators/4, [
		comment is 'Decompiles predicate indicators. All compiled predicate indicators must belong to the same entity, which must be loaded.',
		argnames is ['CompiledPredicateIndicators', 'Entity', 'Type', 'PredicateIndicators']
	]).

	:- public(execution_context/7).
	:- mode(execution_context(?nonvar, ?entity_identifier, ?object_identifier, ?object_identifier, ?object_identifier, @list(callable), @list(callable)), zero_or_one).
	:- info(execution_context/7, [
		comment is 'Execution context term data. Execution context terms should be considered opaque terms subject to change without notice.',
		argnames is ['ExecutionContext', 'Entity', 'Sender', 'This', 'Self', 'MetaCallContext', 'CoinductionStack']
	]).

	print_message(Kind, Component, Message) :-
		message_term_to_tokens(Message, Kind, Component, Tokens),
		(	nonvar(Message),
			message_hook(Message, Kind, Component, Tokens) ->
			% message intercepted; assume that the message is printed
			true
		;	default_print_message(Kind, Component, Tokens)
		).

	% message_term_to_tokens(@term, @term, @term, -list)
	%
	% translates a message term to tokens
	message_term_to_tokens(Message, Kind, Component, Tokens) :-
		(	var(Message) ->
			Tokens = ['Non-instantiated ~q message for component ~q!'-[Kind, Component], nl]
		;	phrase(message_tokens(Message, Component), Tokens) ->
			true
		;	phrase(default_message_tokens(Message), Tokens) ->
			true
		;	Tokens = ['Unknown ~q message for component ~q: ~q'-[Kind, Component, Message], nl]
		).

	% default_print_message(+atom_or_compound, +atom, +list)
	%
	% print a message that was not intercepted by the user
	default_print_message(silent, _, _) :-
		!.
	default_print_message(silent(_), _, _) :-
		!.
	default_print_message(banner, _, _) :-
		\+ current_logtalk_flag(report, on),
		!.
	default_print_message(comment, _, _) :-
		\+ current_logtalk_flag(report, on),
		!.
	default_print_message(comment(_), _, _) :-
		\+ current_logtalk_flag(report, on),
		!.
	default_print_message(warning, _, _) :-
		current_logtalk_flag(report, off),
		!.
	default_print_message(warning(_), _, _) :-
		current_logtalk_flag(report, off),
		!.
	default_print_message(debug, _, _) :-
		current_logtalk_flag(debug, off),
		!.
	default_print_message(debug(_), _, _) :-
		current_logtalk_flag(debug, off),
		!.
	default_print_message(Kind, Component, Tokens) :-
		(	message_prefix_stream(Kind, Component, Prefix, Stream) ->
			true
		;	% no user-defined prefix and stream; use default definition
			default_message_prefix_stream(Kind, Prefix, Stream) ->
			true
		;	% no such kind of message; use "information" instead
			default_message_prefix_stream(information, Prefix, Stream)
		),
		% add begin/2 and end/1 tokens to, respectively, the start and the end of the list of tokens
		% but pass them using discrete arguments instead of doing an expensive list append operation;
		% these two tokens can be intercepted by the user for supporting e.g. message coloring
		(	Tokens = [at_same_line| _] ->
			% continuation message; do not print the prefix
			print_message_tokens_([begin(Kind,Ctx)| Tokens], Stream, Prefix)
		;	print_message_tokens_([begin(Kind,Ctx), Prefix-[]| Tokens], Stream, Prefix)
		),
		print_message_tokens_([end(Ctx)], Stream, Prefix).

	% default_message_prefix_stream(?atom_or_compound, ?atom, ?stream_or_alias)
	%
	% default definitions for any component for the line prefix and output stream used
	% when printing messages; the definitions used here are based on Quintus Prolog and
	% are also used in other Prolog compilers
	default_message_prefix_stream(banner,         '',       user_output).
	default_message_prefix_stream(help,           '',       user_output).
	default_message_prefix_stream(question,       '',       user_output).
	default_message_prefix_stream(question(_),    '',       user_output).
	default_message_prefix_stream(information,    '% ',     user_output).
	default_message_prefix_stream(information(_), '% ',     user_output).
	default_message_prefix_stream(comment,        '% ',     user_output).
	default_message_prefix_stream(comment(_),     '% ',     user_output).
	default_message_prefix_stream(warning,        '*     ', user_error).
	default_message_prefix_stream(warning(_),     '*     ', user_error).
	default_message_prefix_stream(error,          '!     ', user_error).
	default_message_prefix_stream(error(_),       '!     ', user_error).
	default_message_prefix_stream(debug,          '>>> ',   user_error).
	default_message_prefix_stream(debug(_),       '>>> ',   user_error).

	print_message_tokens(Stream, Prefix, Tokens) :-
		(	Tokens = [at_same_line| _] ->
			% continuation message
			print_message_tokens_(Tokens, Stream, Prefix)
		;	Tokens = [begin(Kind, Context)| Rest] ->
			% write the prefix after the begin/2 token
			print_message_tokens_([begin(Kind, Context), Prefix-[]| Rest], Stream, Prefix)
		;	% write first line prefix
			write(Stream, Prefix),
			print_message_tokens_(Tokens, Stream, Prefix)
		).

	% if the list of tokens unifies with (-), assume it's a variable and ignore it
	print_message_tokens_((-), _, _).
	print_message_tokens_([], _, _).
	print_message_tokens_([Token| Tokens], Stream, Prefix) :-
		(	print_message_token(Stream, Prefix, Token, Tokens) ->
			% token printing intercepted by user-defined code
			true
		;	% no user-defined token printing; use Logtalk default
			default_print_message_token(Token, Tokens, Stream, Prefix) ->
			true
		;	% unsupported token
			writeq(Stream, Token)
		),
		print_message_tokens_(Tokens, Stream, Prefix).

	% if a token unifies with (-), assume it's a variable and ignore it
	default_print_message_token((-), _, _, _).
	default_print_message_token(at_same_line, _, _, _).
	default_print_message_token(nl, Tokens, Stream, Prefix) :-
		(	Tokens == [] ->
			nl(Stream)
		;	Tokens = [end(_)] ->
			nl(Stream)
		;	nl(Stream),
			write(Stream, Prefix)
		).
	default_print_message_token(flush, _, Stream, _) :-
		flush_output(Stream).
	default_print_message_token(Format-Arguments, _, Stream, _) :-
		{'$lgt_format'(Stream, Format, Arguments)}.
	default_print_message_token(term(Term, Options), _, Stream, _) :-
		write_term(Stream, Term, Options).
	% the following tokens were first introduced by SWI-Prolog; we use default definitions
	% for compatibility when running Logtalk with other backend Prolog compilers
	default_print_message_token(ansi(_, Format, Arguments), _, Stream, _) :-
		{'$lgt_format'(Stream, Format, Arguments)}.
	default_print_message_token(begin(_, _), _, _, _).
	default_print_message_token(end(_), _, _, _).

	default_message_tokens(@Message) -->
		['~w'-[Message], nl].
	default_message_tokens(Key-Value) -->
		{copy_term(Value, Copy), numbervars(Copy, 0, _)},
		['~w: ~q'-[Key, Copy], nl].
	default_message_tokens(Format+Arguments) -->
		[Format-Arguments, nl].
	default_message_tokens([]) -->
		[].
	default_message_tokens([Item| Items]) -->
		{copy_term([Item| Items], Copy), numbervars(Copy, 0, _)},
		default_message_tokens_list(Copy).
	default_message_tokens(Title::List) -->
		{copy_term(List, Copy), numbervars(Copy, 0, _)},
		['~w:'-[Title], nl],
		default_message_tokens_list(Copy).

	default_message_tokens_list([]) -->
		[].
	default_message_tokens_list([Item| Items]) -->
		['- '-[]], default_message_tokens_list_item(Item),
		default_message_tokens_list(Items).

	default_message_tokens_list_item(@Message) -->
		default_message_tokens(@Message).
	default_message_tokens_list_item(Key-Value) -->
		default_message_tokens(Key-Value).
	default_message_tokens_list_item(Format+Arguments) -->
		default_message_tokens(Format+Arguments).
	default_message_tokens_list_item(Item) -->
		['~q'- [Item], nl].

	ask_question(Kind, Component, Question, Check, Answer) :-
		message_term_to_tokens(Question, Kind, Component, Tokens),
		(	question_hook(Question, Kind, Component, Tokens, Check, Answer) ->
			% question asking intercepted; assume that the question was answered
			true
		;	% print the question text
			default_print_message(Kind, Component, Tokens),
			% find the output stream for printing the question prompt
			(	message_prefix_stream(Kind, Component, _, OutputStream) ->
				true
			;	% no user-defined prefix and stream; use default definition
				default_message_prefix_stream(Kind, _, OutputStream) ->
				true
			;	% no such kind of message; use "information" instead
				default_message_prefix_stream(information, _, OutputStream)
			),
			% find the prompt and the input stream
			(	question_prompt_stream(Kind, Component, Prompt, InputStream) ->
				true
			;	default_question_prompt_stream(Kind, _, Prompt, InputStream) ->
				true
			;	% no such kind of question; use "question" instead
				default_question_prompt_stream(question, _, Prompt, InputStream)
			),
			read_loop(InputStream, OutputStream, Prompt, Check, Answer)
		).

	:- meta_predicate(read_loop(*, *, *, 1, *)).

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		read_loop(InputStream, OutputStream, Prompt, Check, Answer) :-
			setup_call_cleanup(
				prompt(Old, ''),
				(	repeat,
						write(OutputStream, Prompt),
						read(InputStream, Answer),
					call(Check, Answer),
					!
				),
				prompt(_, Old)
			).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		read_loop(InputStream, OutputStream, Prompt, Check, Answer) :-
			prompt(Old, ''),
			call_cleanup(
				(	repeat,
						write(OutputStream, Prompt),
						read(InputStream, Answer),
					call(Check, Answer),
					!
				),
				prompt(_, Old)
			).

	:- else.

		read_loop(InputStream, OutputStream, Prompt, Check, Answer) :-
			repeat,
				write(OutputStream, Prompt),
				read(InputStream, Answer),
			call(Check, Answer),
			!.

	:- endif.

	default_question_prompt_stream(question, _, '> ', user_input).

	expand_library_path(LibraryPath, AbsolutePath) :-
		(	atom(LibraryPath) ->
			{'$lgt_expand_library_alias'(LibraryPath, AbsolutePath)}
		;	callable(LibraryPath),
			LibraryPath =.. [Library, File],
			atom(File),
			{'$lgt_expand_library_alias'(Library, Prefix)},
			atom_concat(Prefix, File, AbsolutePath)
		).

	loaded_file(Path) :-
		{'$lgt_loaded_file_'(Basename, Directory, _, _, _, _, _)},
		atom_concat(Directory, Basename, Path).

	loaded_file_property(Path, Property) :-
		(	var(Path) ->
			{'$lgt_loaded_file_'(Basename, Directory, Mode, Flags, TextProperties, PrologFile, TimeStamp)},
			atom_concat(Directory, Basename, Path)
		;	{'$lgt_loaded_file_'(Basename, Directory, Mode, Flags, TextProperties, PrologFile, TimeStamp)},
			atom_concat(Directory, Basename, Path),
			!
		),
		loaded_file_property(Property, Basename, Directory, Mode, Flags, TextProperties, PrologFile, TimeStamp).

	loaded_file_property(basename(Basename), Basename, _, _, _, _, _, _).
	loaded_file_property(directory(Directory), _, Directory, _, _, _, _, _).
	loaded_file_property(mode(Mode), _, _, Mode, _, _, _, _).
	loaded_file_property(flags(Flags), _, _, _, Flags, _, _, _).
	loaded_file_property(text_properties(TextProperties), _, _, _, _, TextProperties, _, _).
	loaded_file_property(target(PrologFile), _, _, _, _, _, PrologFile, _).
	loaded_file_property(modified(TimeStamp), _, _, _, _, _, _, TimeStamp).
	loaded_file_property(parent(Parent), Basename, Directory, _, _, _, _, _) :-
		atom_concat(Directory, Basename, Path),
		{'$lgt_parent_file_'(Path, Parent)}.
	loaded_file_property(includes(File), Basename, Directory, _, _, _, _, _) :-
		{'$lgt_included_file_'(File, Basename, Directory, _)}.
	loaded_file_property(object(Object), Basename, Directory, _, _, _, _, _) :-
		{'$lgt_current_object_'(Object, _, _, _, _, _, _, _, _, _, _),
		 '$lgt_entity_property_'(Object, file_lines(Basename, Directory, _, _))}.
	loaded_file_property(protocol(Protocol), Basename, Directory, _, _, _, _, _) :-
		{'$lgt_current_protocol_'(Protocol, _, _, _, _),
		 '$lgt_entity_property_'(Protocol, file_lines(Basename, Directory, _, _))}.
	loaded_file_property(category(Category), Basename, Directory, _, _, _, _, _) :-
		{'$lgt_current_category_'(Category, _, _, _, _, _),
		 '$lgt_entity_property_'(Category, file_lines(Basename, Directory, _, _))}.
	loaded_file_property(library(Library), _, Directory, _, _, _, _, _) :-
		logtalk_library_path(Library, _),
		{'$lgt_expand_library_alias'(Library, Directory)}, !.

	file_type_extension(Type, Extension) :-
		{'$lgt_file_extension'(Type, Extension)}.
	file_type_extension(source, Extension) :-
		{'$lgt_file_extension'(logtalk, Extension)}.
	file_type_extension(source, Extension) :-
		{'$lgt_file_extension'(prolog, Extension)}.

	compile_aux_clauses(Clauses) :-
		{'$lgt_compile_aux_clauses'(Clauses)}.

	entity_prefix(Entity, Prefix) :-
		{'$lgt_entity_prefix'(Entity, Prefix)}.

	compile_predicate_heads(Heads, Entity, CompiledHeads, ExecutionContext) :-
		{'$lgt_compile_predicate_heads'(Heads, Entity, CompiledHeads, ExecutionContext)}.

	compile_predicate_indicators(PredicateIndicators, Entity, CompiledPredicateIndicators) :-
		{'$lgt_compile_predicate_indicators'(PredicateIndicators, Entity, CompiledPredicateIndicators)}.

	decompile_predicate_indicators(CompiledPredicateIndicators, Entity, Type, PredicateIndicators) :-
		{'$lgt_decompile_predicate_indicators'(CompiledPredicateIndicators, Entity, Type, PredicateIndicators)}.

	decompile_predicate_heads(THeads, Entity, Type, Heads) :-
		{'$lgt_decompile_predicate_heads'(THeads, Entity, Type, Heads)}.

	execution_context(ExecutionContext, Entity, Sender, This, Self, MetaCallContext, CoinductionStack) :-
		{'$lgt_execution_context'(ExecutionContext, Entity, Sender, This, Self, MetaCallContext, CoinductionStack)}.

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, gnu)).
	% workaround gplc limitation when dealing with multifile predicates
	% that are called from a file but not defined in that file
	:- multifile(logtalk_library_path/2).
	:- dynamic(logtalk_library_path/2).
	:- multifile('$lgt_current_protocol_'/5).
	:- dynamic('$lgt_current_protocol_'/5).
	:- multifile('$lgt_current_category_'/6).
	:- dynamic('$lgt_current_category_'/6).
	:- multifile('$lgt_included_file_'/4).
	:- dynamic('$lgt_included_file_'/4).
:- endif.
