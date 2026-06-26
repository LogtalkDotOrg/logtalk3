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
		set_logtalk_flag(report, warnings),
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
			debug(on),
			source_data(on)
		]),
		logtalk_load(lgtunit(loader)),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- else.

	:- initialization((
		write('(not applicable)'), nl
	)).

:- endif.
