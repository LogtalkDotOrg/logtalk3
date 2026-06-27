%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if((
	current_logtalk_flag(sockets, supported),
	current_prolog_flag(bounded, false)
)).

	:- initialization((
		logtalk_load(basic_types(loader)),
		logtalk_load(base64(loader)),
		logtalk_load(crypto(loader)),
		logtalk_load(hashes(loader)),
		logtalk_load(http_client(loader)),
		logtalk_load(jwt(loader)),
		logtalk_load(json(loader)),
		logtalk_load(options(loader)),
		logtalk_load(process(loader)),
		logtalk_load(url(loader)),
		logtalk_load([
			open_id_helpers,
			open_id_discovery,
			open_id_pkce,
			open_id_logout,
			open_id_response,
			open_id_client,
			open_id_jwks,
			open_id_der,
			open_id_openssl,
			open_id_jwt,
			open_id_jwks_cache,
			open_id
		], [
			optimize(on)
		])
	)).

:- else.

	:- initialization((write('(open_id library not available for your backend Prolog compiler)'), nl)).

:- endif.
