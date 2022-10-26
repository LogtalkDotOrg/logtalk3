%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- category(packs_common).

	:- info([
		version is 0:28:0,
		author is 'Paulo Moura',
		date is 2022-10-26,
		comment is 'Common predicates for the packs tool objects.'
	]).

	:- public(setup/0).
	:- mode(setup, one).
	:- info(setup/0, [
		comment is 'Ensures that registries and packs directory structure exists. Preserves any defined registries and installed packs.'
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets registries and packs directory structure. Deletes any defined registries and installed packs.'
	]).

	:- public(verify_commands_availability/0).
	:- mode(verify_commands_availability, zero_or_one).
	:- info(verify_commands_availability/0, [
		comment is 'Verifies required shell commands availability. Fails printing an error message if a command is missing.'
	]).

	:- public(help/0).
	:- mode(help, one).
	:- info(help/0, [
		comment is 'Provides help about the main predicates.'
	]).

	:- public(pin/1).
	:- mode(pin(+atom), zero_or_one).
	:- info(pin/1, [
		comment is 'Pins a resource (pack or registry) preventing it from being updated, uninstalled, or deleted. Fails if the resource is not found.',
		argnames is ['Resource']
	]).

	:- public(pin/0).
	:- mode(pin, one).
	:- info(pin/0, [
		comment is 'Pins all resource (packs or registries) preventing them from being updated, uninstalled, or deleted. Note that resources added after calling this predicate will not be pinned.'
	]).

	:- public(unpin/1).
	:- mode(unpin(+atom), zero_or_one).
	:- info(unpin/1, [
		comment is 'Unpins a resource (pack or registry), allowing it to be updated, uninstalled, or deleted. Fails if the resource is not found.',
		argnames is ['Resource']
	]).

	:- public(unpin/0).
	:- mode(unpin, one).
	:- info(unpin/0, [
		comment is 'Unpins all resources (packs or registries), allowing them to be updated, uninstalled, or deleted.'
	]).

	:- public(pinned/1).
	:- mode(pinned(+atom), zero_or_one).
	:- info(pinned/1, [
		comment is 'True iff the resource (pack or registry) is defined or installed and if it is pinned.',
		argnames is ['Resource']
	]).

	:- public(directory/2).
	:- mode(directory(?atom, ?atom), zero_or_more).
	:- info(directory/2, [
		comment is 'Enumerates by backtracking all packs or registries and respective installation or definition directories (using the internal backend format).',
		argnames is ['Resource', 'Directory']
	]).

	:- public(directory/1).
	:- mode(directory(?atom), zero_or_one).
	:- info(directory/1, [
		comment is 'Prints the directory where the registry or the pack is installed (using the native operating-system format).',
		argnames is ['Resource']
	]).

	:- public(readme/2).
	:- mode(readme(+atom, -atom), zero_or_one).
	:- info(readme/2, [
		comment is 'Returns the path to the resource (pack or registry) readme file (using the internal backend format). Fails if the resource is not defined or installed or if no readme file is found for it.',
		argnames is ['Resource', 'ReadMeFile']
	]).

	:- public(readme/1).
	:- mode(readme(+atom), zero_or_one).
	:- info(readme/1, [
		comment is 'Prints the path to the resource (pack or registry) readme file (using the native operating-system format). Fails if the resource is not defined or installed or if no readme file is found for it.',
		argnames is ['Resource']
	]).

	:- public(logtalk_packs/1).
	:- mode(logtalk_packs(-atom), one).
	:- info(logtalk_packs/1, [
		comment is 'Returns the directory prefix (using the internal backend format) where the registries, packs, and archives are installed.',
		argnames is ['LogtalkPacks']
	]).

	:- public(logtalk_packs/0).
	:- mode(logtalk_packs, one).
	:- info(logtalk_packs/0, [
		comment is 'Prints the directory prefix (using the native operating-system format) where the registries, packs, and archives are installed.'
	]).

	:- public(prefix/1).
	:- mode(prefix(-atom), one).
	:- info(prefix/1, [
		comment is 'Returns the directory prefix (using the internal backend format) where the registries or packs are installed.',
		argnames is ['Prefix']
	]).

	:- public(prefix/0).
	:- mode(prefix, one).
	:- info(prefix/0, [
		comment is 'Prints the directory prefix (using the native operating-system format) where the registries or packs are installed.'
	]).

	:- protected(readme_file_path/2).
	:- mode(readme_file_path(+atom, -atom), zero_or_one).
	:- info(readme_file_path/2, [
		comment is 'Returns the absolute path for the given directory readme file if it exists.',
		argnames is ['Directory',  'ReadMeFile']
	]).

	:- protected(print_readme_file_path/1).
	:- mode(print_readme_file_path(+atom), one).
	:- info(print_readme_file_path/1, [
		comment is 'Prints the absolute path for the given directory readme file if it exists. Succeeds otherwise.',
		argnames is ['Directory']
	]).

	:- protected(command/2).
	:- mode(command(+atom, @nonvar), zero_or_one).
	:- info(command/2, [
		comment is 'Executes a shell command. Prints an error message and fails if the command fails.',
		argnames is ['Command', 'FailureMessage']
	]).

	:- protected(load_registry/1).
	:- mode(load_registry(+atom), zero_or_one).
	:- info(load_registry/1, [
		comment is 'Loads all registry files from the given directory.',
		argnames is ['Directory']
	]).

	:- protected(tar_command/1).
	:- mode(tar_command(-atom), one).
	:- info(tar_command/1, [
		comment is 'Returns the name of the tar command to be used depending on the operating-system.',
		argnames is ['Command']
	]).

	:- protected(supported_archive/1).
	:- mode(supported_archive(+atom), zero_or_one).
	:- info(supported_archive/1, [
		comment is 'True iff the archive format is supported.',
		argnames is ['Extension']
	]).

	:- protected(supported_url_archive/1).
	:- mode(supported_url_archive(+atom), zero_or_one).
	:- info(supported_url_archive/1, [
		comment is 'True iff the URL archive is supported.',
		argnames is ['URL']
	]).

	:- protected(decode_url_spaces/2).
	:- mode(decode_url_spaces(+atom, -atom), one).
	:- info(decode_url_spaces/2, [
		comment is 'Decodes encoded spaces (%20) in URLs to spaces.',
		argnames is ['URL', 'Decoded']
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(logtalk, [
		expand_library_path/2, print_message/3
	]).

	:- uses(os, [
		environment_variable/2, operating_system_type/1, shell/1,
		file_exists/1, internal_os_path/2, make_directory_path/1,
		path_concat/3, directory_files/3
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	help :-
		print_message(help, packs, help).

	setup :-
		logtalk_packs(LogtalkPacks),
		path_concat(LogtalkPacks, 'registries', Registries),
		make_directory_path(Registries),
		path_concat(LogtalkPacks, 'packs', Packs),
		make_directory_path(Packs),
		path_concat(LogtalkPacks, 'archives', Archives),
		make_directory_path(Archives),
		path_concat(Archives, 'registries', ArchivesRegistries),
		make_directory_path(ArchivesRegistries),
		path_concat(Archives, 'packs', ArchivesPacks),
		make_directory_path(ArchivesPacks).

	reset :-
		logtalk_packs(LogtalkPacks),
		internal_os_path(LogtalkPacks, LogtalkPacksOS),
		(	operating_system_type(windows) ->
			atomic_list_concat(['del /f /s /q "', LogtalkPacksOS, '" > nul && rmdir /s /q "', LogtalkPacksOS, '" > nul'], Command)
		;	% assume unix
			atomic_list_concat(['rm -rf "',  LogtalkPacksOS, '"'], Command)
		),
		command(Command, reset_failed(LogtalkPacks)),
		setup.

	logtalk_packs(LogtalkPacks) :-
		(	expand_library_path(logtalk_packs, LogtalkPacks) ->
			true
		;	environment_variable('LOGTALKPACKS', LogtalkPacks0) ->
			internal_os_path(LogtalkPacks, LogtalkPacks0)
		;	expand_library_path(home('logtalk_packs/'), LogtalkPacks)
		).

	logtalk_packs :-
		logtalk_packs(LogtalkPacks),
		internal_os_path(LogtalkPacks, OSLogtalkPacks),
		print_message(information, packs, logtalk_packs(OSLogtalkPacks)).

	verify_commands_availability :-
		operating_system_type(OS),
		verify_commands_availability(OS).

	verify_commands_availability(unix) :-
		command('type curl >/dev/null 2>&1', missing_command(curl)),
		command('type bsdtar >/dev/null 2>&1', missing_command(bsdtar)),
		command('type sha256sum >/dev/null 2>&1', missing_command(sha256sum)),
		command('type gpg >/dev/null 2>&1', missing_command(gpg)).
	verify_commands_availability(windows) :-
		command('where /q curl.exe', missing_command(curl)),
		command('where /q tar.exe', missing_command(tar)),
		command('where /q certutil.exe', missing_command(sha256sum)),
		command('where /q gpg.exe', missing_command(gpg)).
	verify_commands_availability(unknown) :-
		verify_commands_availability(unix).

	load_registry(Directory) :-
		(	path_concat(Directory, 'loader.logtalk',  Loader),
			file_exists(Loader) ->
			load_registry_files(Directory, Loader)
		;	path_concat(Directory, 'loader.lgt',  Loader),
			file_exists(Loader) ->
			load_registry_files(Directory, Loader)
		;	print_message(warning, packs, registry_loader_file_missing(Directory)),
			fail
		).

	load_registry_files(Directory, Loader) :-
		(	logtalk_load(Loader, [reload(always), source_data(on), hook(registry_loader_hook)]) ->
			true
		;	print_message(warning, packs, registry_loading_failed(Directory))
		).

	command(Command, FailureMessage) :-
		(	shell(Command) ->
			true
		;	print_message(error, packs, FailureMessage),
			fail
		).

	tar_command(Tar) :-
		operating_system_type(OS),
		tar_command(OS, Tar).

	tar_command(unix, bsdtar).
	tar_command(windows, tar).
	tar_command(unknown, tar).

	supported_archive('.zip').
	supported_archive('.tgz').
	supported_archive('.tar.gz').
	supported_archive('.tbz2').
	supported_archive('.tar.bz2').

	supported_url_archive(URL) :-
		supported_archive(Extension),
		sub_atom(URL, _, _, 0, Extension),
		!.

	% use directory_files/2 instead of file_exists/1 as the later may
	% succeed for a file that differs in case in operating-systems
	% like macOS that are not case-sensitive but are case-preserving
	readme_file_path(Directory, ReadMeFile) :-
		readme_file_name(Basename),
		directory_files(Directory, Files, [type(regular), dot_files(false)]),
		member(Basename, Files),
		!,
		path_concat(Directory, Basename, ReadMeFile).

	print_readme_file_path(Directory) :-
		(	readme_file_path(Directory, ReadMeFile) ->
			internal_os_path(ReadMeFile, OSReadMeFile),
			print_message(comment, packs, readme_file(OSReadMeFile))
		;	true
		).

	readme_file_name('README.MD').
	readme_file_name('README.md').
	readme_file_name('Readme.md').
	readme_file_name('readme.md').
	readme_file_name('README').
	readme_file_name('README.TXT').
	readme_file_name('README.txt').
	readme_file_name('Readme.txt').
	readme_file_name('readme.txt').

	decode_url_spaces(URL0, URL) :-
		atom_chars(URL0, Chars0),
		decode_url_spaces_in_list(Chars0, Chars),
		atom_chars(URL, Chars).

	decode_url_spaces_in_list([], []).
	decode_url_spaces_in_list(['%','2','0'| Chars0], [' '| Chars]) :-
		!,
		decode_url_spaces_in_list(Chars0, Chars).
	decode_url_spaces_in_list([Char| Chars0], [Char| Chars]) :-
		decode_url_spaces_in_list(Chars0, Chars).

	:- multifile(type::type/1).
	type::type(pack_version).

	% add the actual checking code for the new type
	:- multifile(type::check/2).
	type::check(pack_version, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term = Major:Minor:Patch,
			integer(Major), Major >= 0,
			integer(Minor), Minor >= 0,
			integer(Patch), Patch >= 0
		->	true
		;	throw(type_error(pack_version, Term))
		).

:- end_category.
