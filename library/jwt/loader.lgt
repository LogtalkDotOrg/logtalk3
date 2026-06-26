%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if((
	current_prolog_flag(bounded, false)
)).

	:- initialization((
		logtalk_load(base64(loader)),
		logtalk_load(crypto(loader)),
		logtalk_load(hashes(loader)),
		logtalk_load(hmac(loader)),
		logtalk_load(json(loader)),
		logtalk_load(options(loader)),
		logtalk_load(os(loader)),
		logtalk_load([
			jwt_helpers,
			jwt_compact,
			jwt_jwa,
			jwt_claims,
			jwt_der,
			jwt_openssl,
			jwt_jwk,
			jwt_jwks,
			jwt_jws,
			jwt
		], [
			optimize(on)
		])
	)).

:- else.

	:- initialization((write('(jwt library not available for your backend Prolog compiler)'), nl)).

:- endif.
