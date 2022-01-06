%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Jacob Friedman <jfriedman@graphstax.com>
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


:- category(vscode_message_streamer).

	:- info([
		version is 0:1:0,
		author is 'Jacob Friedman',
		date is 2021-08-28,
		comment is 'Rewrite compiler error and warnings messages for parsing with Visual Studio Code.'
	]).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	% allow default processing of the messages
	logtalk::message_hook(_Message, error, core, Tokens) :-
		message_hook(Tokens, error),
		fail.
	logtalk::message_hook(_Message, error(Class), core, Tokens) :-
		message_hook(Tokens, error(Class)),
		fail.
	logtalk::message_hook(_Message, warning, core, Tokens) :-
		message_hook(Tokens, warning),
		fail.
	logtalk::message_hook(_Message, warning(Class), core, Tokens) :-
		message_hook(Tokens, warning(Class)),
		fail.

	% Considerations: We CANNOT:
	% 1. Use sockets, because not everyone runs on Unix!
	% 2. Take up an inet port, because that would be a nuisance (and 1).
	% 2.a. Use a localserver, because (2 and 1). Also, Prolog on localhost.. not yet.
	% 3. Use the local file-directory as a scratch.
	% 4. Clog up processes trying to talk to node.
	%
	% Thus, we'll just use a logfile and get Node to read from it...
	% in some classic-js-magical way.

	message_hook(Tokens, Kind) :-
		logtalk::expand_library_path(logtalk_user('scratch/.messages'), File),
		open(File, append, Stream),
		logtalk::message_prefix_stream(Kind, core, Prefix, user_error),
		logtalk::print_message_tokens(Stream, Prefix, Tokens),
		close(Stream).

:- end_category.
