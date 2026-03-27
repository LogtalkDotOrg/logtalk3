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


:- object(linda,
	imports((linda_server, linda_client))).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-03-27,
		comment is 'Linda tuple-space implementation for process communication. Provides a server that acts as a shared blackboard where clients can write (``out/1-2``), read (``rd/1-2``), and remove (``in/1-2``) tuples. Uses threaded engines for the server implementation and the sockets library for network communication.',
		remarks is [
			'Supported backends' - 'SWI-Prolog, Trealla Prolog, and XVM (requires both multi-threading and sockets support).',
			'Linda operations' - 'The basic operations are ``out/1-2`` (write tuple), ``in/1-2`` (remove tuple, blocking), ``rd/1-2`` (read tuple, blocking), ``in_noblock/1-2`` (remove tuple, non-blocking), and ``rd_noblock/1-2`` (read tuple, non-blocking).',
			'Tuple matching' - 'Tuples are matched using unification.',
			'Blocking behavior' - 'The ``in/1-2`` and ``rd/1-2`` predicates block until a matching tuple is available. The ``in_noblock/1-2`` and ``rd_noblock/1-2`` predicates fail immediately if no matching tuple is found.',
			'Multiple clients' - 'Multiple clients can connect to the same server. A tuple removed by the ``in/1-2`` or ``in_noblock/1-2`` predicates is only removed for one client.',
			'Multiple servers' - 'A client can connect to multiple servers. The first server it connects to, uses by default the ``blackboard`` alias.',
			'API compatibility' - 'The API is inspired by the SICStus Prolog Linda library.',
			'Network communication' - 'Uses TCP sockets for client-server communication, allowing processes to run on different machines.'
		]
	]).

	:- threaded.

:- end_object.
