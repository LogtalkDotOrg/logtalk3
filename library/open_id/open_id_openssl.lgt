%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_openssl).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-17,
		comment is 'OpenSSL-backed JWT signature verification helpers.'
	]).

	:- public(verify/5).
	:- mode(verify(+atom, +atom, +atom, +list(byte), +list(compound)), zero_or_one_or_error).
	:- info(verify/5, [
		comment is 'Verifies a JWT signing input and signature using OpenSSL and a PEM public key.',
		argnames is ['Algorithm', 'PEM', 'SigningInput', 'Signature', 'Options'],
		exceptions is [
			'The OpenSSL executable does not exist' - existence_error(os_command, 'Executable'),
			'``Signature`` is not a 64-byte raw ES256 signature' - domain_error(open_id_es256_signature, 'Signature')
		]
	]).

	:- uses(crypto, [
		random_bytes/2, hex_bytes/2
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(os, [
		delete_file/1, path_concat/3, resolve_command_path/2, temporary_directory/1, internal_os_path/2
	]).

	:- uses(process, [
		create/3, wait/2
	]).

	verify(Algorithm, PEM, SigningInput, Signature0, Options) :-
		openssl_executable(Options, OpenSSL),
		signature_bytes(Algorithm, Signature0, Signature),
		temporary_files(PublicKeyFile, DataFile, SignatureFile),
		write_verification_files(PublicKeyFile, PEM, DataFile, SigningInput, SignatureFile, Signature),
		(	catch(
				run_openssl_verify(OpenSSL, PublicKeyFile, DataFile, SignatureFile),
				Error,
				(cleanup_temporary_files([PublicKeyFile, DataFile, SignatureFile]), throw(Error))
			) ->
			cleanup_temporary_files([PublicKeyFile, DataFile, SignatureFile])
		;	cleanup_temporary_files([PublicKeyFile, DataFile, SignatureFile]),
			fail
		).

	openssl_executable(Options, Path) :-
		(	member(openssl_executable(Executable), Options) ->
			true
		;	Executable = openssl
		),
		(	resolve_command_path(Executable, Path) ->
			true
		;	existence_error(os_command, Executable)
		).

	signature_bytes('RS256', Signature, Signature).
	signature_bytes('ES256', Signature, DER) :-
		open_id_der::es256_signature_der(Signature, DER).

	temporary_files(PublicKeyFile, DataFile, SignatureFile) :-
		temporary_directory(Directory),
		random_bytes(8, Bytes),
		hex_bytes(Prefix, Bytes),
		path_concat(Directory, Prefix, Base),
		atom_concat(Base, '_openid_key.pem', PublicKeyFile),
		atom_concat(Base, '_openid_data.bin', DataFile),
		atom_concat(Base, '_openid_sig.bin', SignatureFile).

	write_verification_files(PublicKeyFile, PEM, DataFile, SigningInput, SignatureFile, Signature) :-
		write_atom_file(PublicKeyFile, PEM),
		write_atom_file(DataFile, SigningInput),
		write_bytes_file(SignatureFile, Signature).

	run_openssl_verify(OpenSSL, PublicKeyFile, DataFile, SignatureFile) :-
		internal_os_path(PublicKeyFile, PublicKeyFileOS),
		internal_os_path(DataFile, DataFileOS),
		internal_os_path(SignatureFile, SignatureFileOS),
		create(
			OpenSSL,
			['dgst', '-sha256', '-verify', PublicKeyFileOS, '-signature', SignatureFileOS, DataFileOS],
			[stdout(Output), stderr(Error), process(Process)]
		),
		wait(Process, Status),
		catch(close(Output), _, true),
		catch(close(Error), _, true),
		once((Status == 0; Status == exit(0))).

	cleanup_temporary_files([]).
	cleanup_temporary_files([File| Files]) :-
		catch(delete_file(File), _, true),
		cleanup_temporary_files(Files).

	write_atom_file(File, Atom) :-
		atom_codes(Atom, Codes),
		open(File, write, Stream, [type(binary)]),
		write_codes(Codes, Stream),
		close(Stream).

	write_bytes_file(File, Bytes) :-
		open(File, write, Stream, [type(binary)]),
		write_bytes(Bytes, Stream),
		close(Stream).

	write_codes([], _Stream).
	write_codes([Code| Codes], Stream) :-
		put_byte(Stream, Code),
		write_codes(Codes, Stream).

	write_bytes([], _Stream).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

:- end_object.
