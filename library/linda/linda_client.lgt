%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- category(linda_client,
	extends(options)).

	:- info([
		version is 2:0:1,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Linda client predicates and client-side connection state. Import into a threaded object together with the linda_server category.'
	]).


	% ==========================================================================
	% Client predicates
	% ==========================================================================

	:- public(linda_client/2).
	:- mode(linda_client(++address, ++list(compound)), one_or_error).
	:- info(linda_client/2, [
		comment is 'Connects to a Linda server at the given address (``Host:Port``) using the given options.',
		argnames is ['Address', 'Options'],
		exceptions is [
			'Already connected' - linda_error(already_connected),
			'Connection failed' - linda_error(connection_failed('Error'))
		],
		remarks is [
			'Option ``alias(Alias)``' - 'Use ``Alias`` as an alias to the server address. Default is ``blackboard``.'
		]
	]).

	:- public(linda_client/1).
	:- mode(linda_client(++address), one_or_error).
	:- info(linda_client/1, [
		comment is 'Connects to a Linda server at the given address (``Host:Port``) using default options.',
		argnames is ['Address'],
		exceptions is [
			'Already connected' - linda_error(already_connected),
			'Connection failed' - linda_error(connection_failed('Error'))
		]
	]).

	:- public(close_client/1).
	:- mode(close_client(++address_or_alias), one).
	:- info(close_client/1, [
		comment is 'Closes the connection to the given Linda server.',
		argnames is ['Address']
	]).

	:- public(shutdown_server/1).
	:- mode(shutdown_server(++address_or_alias), one_or_error).
	:- info(shutdown_server/1, [
		comment is 'Sends a shutdown signal to the given server. The server stops accepting new connections but continues serving existing clients until they all disconnect. Call ``close_client/1`` after this predicate.',
		argnames is ['Address'],
		exceptions is [
			'Not connected' - linda_error(not_connected('Address'))
		]
	]).

	:- public(linda_timeout/2).
	:- mode(linda_timeout(?compound, +compound), one).
	:- info(linda_timeout/2, [
		comment is 'Gets or sets the client timeout. ``OldTime`` is unified with the current timeout and the timeout is set to ``NewTime``. The timeout value is either ``off`` (no timeout, wait forever) or ``Seconds:Milliseconds``.',
		argnames is ['OldTime', 'NewTime']
	]).

	% ==========================================================================
	% Tuple-space operations (client-side)
	% ==========================================================================

	:- public(out/2).
	:- mode(out(++address_or_alias, +term), one).
	:- info(out/2, [
		comment is 'Places the tuple ``Tuple`` in the tuple-space of the given server.',
		argnames is ['AddressOrALias', 'Tuple']
	]).

	:- public(out/1).
	:- mode(out(+term), one).
	:- info(out/1, [
		comment is 'Places the tuple ``Tuple`` in the tuple-space of the default server.',
		argnames is ['Tuple']
	]).

	:- public(in/2).
	:- mode(in(++address_or_alias, ?term), one).
	:- info(in/2, [
		comment is 'Removes a tuple matching ``Tuple`` from the tuple-space of the given server. Blocks if no matching tuple is available.',
		argnames is ['AddressOrALias', 'Tuple']
	]).

	:- public(in/1).
	:- mode(in(?term), one).
	:- info(in/1, [
		comment is 'Removes a tuple matching ``Tuple`` from the tuple-space of the default server. Blocks if no matching tuple is available.',
		argnames is ['Tuple']
	]).

	:- public(in_noblock/2).
	:- mode(in_noblock(++address_or_alias, ?term), zero_or_one).
	:- info(in_noblock/2, [
		comment is 'Removes a tuple matching ``Tuple`` from the tuple-space of the default server. Fails if no matching tuple is available.',
		argnames is ['AddressOrALias', 'Tuple']
	]).

	:- public(in_noblock/1).
	:- mode(in_noblock(?term), zero_or_one).
	:- info(in_noblock/1, [
		comment is 'Removes a tuple matching ``Tuple`` from the tuple-space of the default server. Fails if no matching tuple is available.',
		argnames is ['Tuple']
	]).

	:- public(in_list/3).
	:- mode(in_list(++address_or_alias, +list, ?term), one).
	:- info(in_list/3, [
		comment is 'Removes a tuple matching one of the patterns in ``TupleList`` from the tuple-space of the given server. ``Tuple`` is unified with the matched tuple. Blocks if no matching tuple is available.',
		argnames is ['AddressOrALias', 'TupleList', 'Tuple']
	]).

	:- public(in_list/2).
	:- mode(in_list(+list, ?term), one).
	:- info(in_list/2, [
		comment is 'Removes a tuple matching one of the patterns in ``TupleList`` from the tuple-space of the default server. ``Tuple`` is unified with the matched tuple. Blocks if no matching tuple is available.',
		argnames is ['TupleList', 'Tuple']
	]).

	:- public(rd/2).
	:- mode(rd(++address_or_alias, ?term), one).
	:- info(rd/2, [
		comment is 'Reads a tuple matching ``Tuple`` from the tuple-space of the given server without removing it. Blocks if no matching tuple is available.',
		argnames is ['AddressOrALias', 'Tuple']
	]).

	:- public(rd/1).
	:- mode(rd(?term), one).
	:- info(rd/1, [
		comment is 'Reads a tuple matching ``Tuple`` from the tuple-space of the default server without removing it. Blocks if no matching tuple is available.',
		argnames is ['Tuple']
	]).

	:- public(rd_noblock/2).
	:- mode(rd_noblock(++address_or_alias, ?term), zero_or_one).
	:- info(rd_noblock/2, [
		comment is 'Reads a tuple matching ``Tuple`` from the tuple-space of the given server without removing it. Fails if no matching tuple is available.',
		argnames is ['AddressOrALias', 'Tuple']
	]).

	:- public(rd_noblock/1).
	:- mode(rd_noblock(?term), zero_or_one).
	:- info(rd_noblock/1, [
		comment is 'Reads a tuple matching ``Tuple`` from the tuple-space of the default server without removing it. Fails if no matching tuple is available.',
		argnames is ['Tuple']
	]).

	:- public(rd_list/3).
	:- mode(rd_list(++address_or_alias, +list, ?term), one).
	:- info(rd_list/3, [
		comment is 'Reads a tuple matching one of the patterns in ``TupleList`` from the tuple-space of the given server without removing it. ``Tuple`` is unified with the matched tuple. Blocks if no matching tuple is available.',
		argnames is ['AddressOrALias', 'TupleList', 'Tuple']
	]).

	:- public(rd_list/2).
	:- mode(rd_list(+list, ?term), one).
	:- info(rd_list/2, [
		comment is 'Reads a tuple matching one of the patterns in ``TupleList`` from the tuple-space of the default server without removing it. ``Tuple`` is unified with the matched tuple. Blocks if no matching tuple is available.',
		argnames is ['TupleList', 'Tuple']
	]).

	:- public(findall_rd_noblock/4).
	:- mode(findall_rd_noblock(++address_or_alias, ?term, +term, ?list), one).
	:- info(findall_rd_noblock/4, [
		comment is 'Returns a list of all instances of ``Template`` for tuples matching ``Tuple`` in the tuple-space. The operation is atomic.',
		argnames is ['AddressOrALias', 'Template', 'Tuple', 'List']
	]).

	:- public(findall_rd_noblock/3).
	:- mode(findall_rd_noblock(?term, +term, ?list), one).
	:- info(findall_rd_noblock/3, [
		comment is 'Returns a list of all instances of ``Template`` for tuples matching ``Tuple`` in the tuple-space. The operation is atomic.',
		argnames is ['Template', 'Tuple', 'List']
	]).

	:- public(findall_in_noblock/4).
	:- mode(findall_in_noblock(++address_or_alias, ?term, +term, ?list), one).
	:- info(findall_in_noblock/4, [
		comment is 'Removes and returns a list of all instances of ``Template`` for tuples matching ``Tuple`` in the tuple-space. The operation is atomic - all matching tuples are removed in one synchronized operation.',
		argnames is ['AddressOrALias', 'Template', 'Tuple', 'List']
	]).

	:- public(findall_in_noblock/3).
	:- mode(findall_in_noblock(?term, +term, ?list), one).
	:- info(findall_in_noblock/3, [
		comment is 'Removes and returns a list of all instances of ``Template`` for tuples matching ``Tuple`` in the tuple-space. The operation is atomic - all matching tuples are removed in one synchronized operation.',
		argnames is ['Template', 'Tuple', 'List']
	]).

	% ==========================================================================
	% Private client state
	% ==========================================================================

	:- private(client_connection_input_/2).
	:- dynamic(client_connection_input_/2).
	:- mode(client_connection_input_(?address, ?stream), zero_or_more).
	:- info(client_connection_input_/2, [
		comment is 'Stores the input stream for the client connection to the server.',
		argnames is ['Address', 'InputStream']
	]).

	:- private(client_connection_output_/2).
	:- dynamic(client_connection_output_/2).
	:- mode(client_connection_output_(?address, ?stream), zero_or_more).
	:- info(client_connection_output_/2, [
		comment is 'Stores the output stream for the client connection to the server.',
		argnames is ['Address', 'OutputStream']
	]).

	:- private(client_connection_alias_/2).
	:- dynamic(client_connection_alias_/2).
	:- mode(client_connection_alias_(?address, ?atom), zero_or_more).
	:- info(client_connection_alias_/2, [
		comment is 'Stores the client connection server aliases.',
		argnames is ['Address', 'Alias']
	]).

	:- private(client_timeout_/1).
	:- dynamic(client_timeout_/1).
	:- mode(client_timeout_(?compound), zero_or_one).
	:- info(client_timeout_/1, [
		comment is 'Stores the timeout value for blocking client operations. Value is either ``off`` or ``Seconds:Milliseconds``.',
		argnames is ['Timeout']
	]).

	:- uses(os, [
		cpu_time/1, sleep/1
	]).

	% ==========================================================================
	% Client implementation
	% ==========================================================================

	linda_client(Host:Port, UserOptions) :-
		context(Context),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	client_connection_input_(Host:Port, _) ->
			throw(error(linda_error(already_connected), Context))
		;	catch(
				(	socket::client_open(Host, Port, Input, Output, [type(text)]),
					assertz(client_connection_input_(Host:Port, Input)),
					assertz(client_connection_output_(Host:Port, Output)),
					^^option(alias(Alias), Options),
					assertz(client_connection_alias_(Host:Port, Alias))
				),
				Error,
				throw(error(linda_error(connection_failed(Error)), Context))
			)
		).

	linda_client(Host:Port) :-
		linda_client(Host:Port, []).

	close_client(AddressOrALias) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	retract(client_connection_input_(Address, Input)),
			retract(client_connection_output_(Address, Output)),
			retract(client_connection_alias_(Address, _Alias)) ->
			write_out(Output, exit),
			catch(socket::close(Input, Output), _, true)
		;	true
		).

	shutdown_server(AddressOrALias) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output),
			write_out(Output, shutdown),
			client_connection_input_(Address, Input),
			read_in(Input, Response),
			Response == ok ->
			true
		;	throw(error(linda_error(shutdown_failed(Response)), Context))
		).

	linda_timeout(OldTime, NewTime) :-
		(	retract(client_timeout_(OldTime)) ->
			true
		;	OldTime = off
		),
		assertz(client_timeout_(NewTime)).

	% ==========================================================================
	% Client tuple-space operations
	% ==========================================================================

	out(AddressOrALias, Tuple) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output) ->
			write_out(Output, out(Tuple)),
			client_connection_input_(Address, Input),
			read_in(Input, Response),
			(	Response == ok ->
				true
			;	throw(error(linda_error(out_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected(Address)), Context))
		).

	out(Tuple) :-
		out(blackboard, Tuple).

	in(AddressOrALias, Tuple) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output) ->
			write_out(Output, in(Tuple)),
			client_connection_input_(Address, Input),
			wait_for_result(Input, Result, Context),
			(	Result = result(Match) ->
				Tuple = Match
			;	throw(error(linda_error(in_failed(Result)), Context))
			)
		;	throw(error(linda_error(not_connected(Address)), Context))
		).

	in(Tuple) :-
		in(blackboard, Tuple).

	in_noblock(AddressOrALias, Tuple) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output) ->
			write_out(Output, in_noblock(Tuple)),
			client_connection_input_(Address, Input),
			read_in(Input, Response),
			(	Response = result(Match) ->
				Tuple = Match
			;	Response == fail ->
				fail
			;	throw(error(linda_error(in_noblock_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected(Address)), Context))
		).

	in_noblock(Tuple) :-
		in_noblock(blackboard, Tuple).

	in_list(AddressOrALias, TupleList, Tuple) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output) ->
			write_out(Output, in_list(TupleList)),
			client_connection_input_(Address, Input),
			wait_for_result(Input, Result, Context),
			(	Result = result(Match) ->
				Tuple = Match
			;	throw(error(linda_error(in_failed(Result)), Context))
			)
		;	throw(error(linda_error(not_connected(Address)), Context))
		).

	in_list(TupleList, Tuple) :-
		in_list(blackboard, TupleList, Tuple).

	rd(AddressOrALias, Tuple) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output) ->
			write_out(Output, rd(Tuple)),
			client_connection_input_(Address, Input),
			wait_for_result(Input, Result, Context),
			(	Result = result(Match) ->
				Tuple = Match
			;	throw(error(linda_error(rd_failed(Result)), Context))
			)
		;	throw(error(linda_error(not_connected(Address)), Context))
		).

	rd(Tuple) :-
		rd(blackboard, Tuple).

	rd_noblock(AddressOrALias, Tuple) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output) ->
			write_out(Output, rd_noblock(Tuple)),
			client_connection_input_(Address, Input),
			read_in(Input, Response),
			(	Response = result(Match) ->
				Tuple = Match
			;	Response == fail ->
				fail
			;	throw(error(linda_error(rd_noblock_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected(Address)), Context))
		).

	rd_noblock(Tuple) :-
		rd_noblock(blackboard, Tuple).

	rd_list(AddressOrALias, TupleList, Tuple) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output) ->
			write_out(Output, rd_list(TupleList)),
			client_connection_input_(Address, Input),
			wait_for_result(Input, Result, Context),
			(	Result = result(Match) ->
				Tuple = Match
			;	throw(error(linda_error(rd_failed(Result)), Context))
			)
		;	throw(error(linda_error(not_connected(Address)), Context))
		).

	rd_list(TupleList, Tuple) :-
		rd_list(blackboard, TupleList, Tuple).

	findall_rd_noblock(AddressOrALias, Template, Tuple, Bag) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output) ->
			write_out(Output, findall_rd_noblock(Template, Tuple)),
			client_connection_input_(Address, Input),
			read_in(Input, Response),
			(	Response = result(ResultBag) ->
				Bag = ResultBag
			;	Response == fail ->
				fail
			;	throw(error(linda_error(findall_rd_noblock_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected(Address)), Context))
		).

	findall_rd_noblock(Template, Tuple, Bag) :-
		findall_rd_noblock(blackboard, Template, Tuple, Bag).

	findall_in_noblock(AddressOrALias, Template, Tuple, Bag) :-
		context(Context),
		resolve_alias(AddressOrALias, Address, Context),
		(	client_connection_output_(Address, Output) ->
			write_out(Output, findall_in_noblock(Template, Tuple)),
			client_connection_input_(Address, Input),
			read_in(Input, Response),
			(	Response = result(ResultBag) ->
				Bag = ResultBag
			;	Response == fail ->
				fail
			;	throw(error(linda_error(findall_in_noblock_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected(Address)), Context))
		).

	findall_in_noblock(Template, Tuple, Bag) :-
		findall_in_noblock(blackboard, Template, Tuple, Bag).

	% Check if stream has data ready (backend dependent)
	stream_ready(Stream) :-
		catch(peek_char(Stream, _), _, fail).

	% Auxiliary predicate to wait for result with optional timeout
	wait_for_result(Input, Result, Context) :-
		(	client_timeout_(Timeout), Timeout \== off ->
			Timeout = Seconds:Milliseconds,
			TotalSeconds is Seconds + Milliseconds / 1000,
			wait_for_result_timeout(Input, Result, TotalSeconds, Context)
		;	% No timeout, block until result
			read_in(Input, Result)
		).

	wait_for_result_timeout(Input, Result, TimeoutSeconds, Context) :-
		cpu_time(StartTime),
		wait_for_result_loop(Input, Result, StartTime, TimeoutSeconds, Context).

	wait_for_result_loop(Input, Result, StartTime, TimeoutSeconds, Context) :-
		(	stream_ready(Input) ->
			read_in(Input, Result)
		;	cpu_time(CurrentTime),
			Elapsed is CurrentTime - StartTime,
			(	Elapsed > TimeoutSeconds ->
				throw(error(linda_error(timeout), Context))
			;	% Small sleep to avoid busy waiting
				sleep(0.01),
				wait_for_result_loop(Input, Result, StartTime, TimeoutSeconds, Context)
			)
		).

	resolve_alias(AddressOrALias, Address, Context) :-
		(	var(AddressOrALias) ->
			throw(error(instantiation_error, Context))
		;	\+ atom(AddressOrALias) ->
			Address = AddressOrALias
		;	client_connection_alias_(Address, AddressOrALias) ->
			true
		;	client_connection_input_(_, _) ->
			throw(error(linda_error(not_connected(AddressOrALias)), Context))
		;	Address = AddressOrALias
		).

	default_option(alias(blackboard)).

	valid_option(alias(Alias)) :-
		atom(Alias).

	:- if(current_logtalk_flag(prolog_dialect, xvm)).
		% streams are thread-owned in XVM

		write_out(Output, Term) :-
			{adopt_stream(Output)},
			write_canonical(Output, Term),
			write(Output, '.\n'),
			flush_output(Output).

		read_in(Input, Term) :-
			{adopt_stream(Input)},
			read_term(Input, Term, []).

	:- else.

		write_out(Output, Term) :-
			write_canonical(Output, Term),
			write(Output, '.\n'),
			flush_output(Output).

		read_in(Input, Term) :-
			read_term(Input, Term, []).

	:- endif.

:- end_category.
