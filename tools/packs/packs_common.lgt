%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:12:0,
		author is 'Paulo Moura',
		date is 2021-10-18,
		comment is 'Common predicates for the packs tool objects.'
	]).

	:- public(setup/0).
	:- mode(setup, one).
	:- info(setup/0, [
		comment is 'Setup registries and packs directory structure.'
	]).

	:- public(verify_commands_availability/0).
	:- mode(verify_commands_availability, one).
	:- info(verify_commands_availability/0, [
		comment is 'Verify required shell commands availability.'
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

	:- public(unpin/1).
	:- mode(unpin(+atom), zero_or_one).
	:- info(unpin/1, [
		comment is 'Unpins a resource (pack or registry), allowing it to be updated, uninstalled, or deleted. Fails if the resource is not found.',
		argnames is ['Resource']
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
		comment is 'Enumerates by backtracking all packs or registries and respective installation or definition directories.',
		argnames is ['Resource', 'Directory']
	]).

	:- public(directory/1).
	:- mode(directory(?atom), zero_or_one).
	:- info(directory/1, [
		comment is 'Returns the directory where the registries or the packs are installed.',
		argnames is ['Directory']
	]).

	:- public(readme/2).
	:- mode(readme(+atom, -atom), zero_or_one).
	:- info(readme/2, [
		comment is 'Returns the path to the resource (pack or registry) readme file. Fails if the resource is not defined or installed or if no readme file is found for it.',
		argnames is ['Resource', 'ReadMeFile']
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
		comment is 'Loads all registry files.',
		argnames is ['Path']
	]).

	:- protected(tar_command/1).
	:- mode(tar_command(-atom), one).
	:- info(tar_command/1, [
		comment is 'Returns the name of the tar command to be used.',
		argnames is ['Tar']
	]).

	:- protected(supported_archive/1).
	:- mode(supported_archive(+atom), zero_or_one).
	:- info(supported_archive/1, [
		comment is 'True iff the archive format is supported.',
		argnames is ['Extension']
	]).

	:- uses(logtalk, [
		expand_library_path/2, print_message/3
	]).

	:- uses(os, [
		ensure_file/1, file_exists/1, make_directory_path/1,
		operating_system_type/1, path_concat/3, shell/1
	]).

	help :-
		print_message(help, packs, help).

	setup :-
		expand_library_path(logtalk_packs, LogtalkPacks),
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

	verify_commands_availability :-
		operating_system_type(OS),
		verify_commands_availability(OS).

	verify_commands_availability(unix) :-
		command('which curl > /dev/null', missing_command(curl)),
		command('which bsdtar > /dev/null', missing_command(bsdtar)),
		command('which sha256sum > /dev/null', missing_command(sha256sum)).
	verify_commands_availability(windows) :-
		command('where curl.exe', missing_command(curl)),
		command('where tar.exe', missing_command(tar)),
		command('where certutil.exe', missing_command(sha256sum)).
	verify_commands_availability(unknown) :-
		verify_commands_availability(unix).

	load_registry(Registry) :-
		(	path_concat(Registry, 'loader.logtalk',  Loader),
			file_exists(Loader) ->
			load_registry_files(Registry, Loader)
		;	path_concat(Registry, 'loader.lgt',  Loader),
			file_exists(Loader) ->
			load_registry_files(Registry, Loader)
		;	print_message(warning, packs, registry_loader_file_missing(Registry)),
			fail
		).

	load_registry_files(Registry, Loader) :-
		(	logtalk_load(Loader, [source_data(on), hook(registry_loader_hook)]) ->
			true
		;	print_message(warning, packs, registry_loading_failed(Registry))
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
	supported_archive('.gz').
	supported_archive('.bz2').
	supported_archive('.tgz').
	supported_archive('.tbz').
	supported_archive('.tz2').
	supported_archive('.tbz2').

	readme_file_path(Directory, ReadMeFile) :-
		readme_file_name(Basename),
		path_concat(Directory, Basename, ReadMeFile),
		file_exists(ReadMeFile),
		!.

	print_readme_file_path(Directory) :-
		(	readme_file_path(Directory, ReadMeFile) ->
			print_message(comment, packs, readme_file(ReadMeFile))
		;	true
		).

	readme_file_name('README.md').
	readme_file_name('Readme.md').
	readme_file_name('readme.md').
	readme_file_name('README').

:- end_category.
