%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(osp).

	:- info([
		version is 1:35:0,
		author is 'Paulo Moura',
		date is 2023-07-31,
		comment is 'Portable operating-system access protocol.',
		remarks is [
			'Error handling' - 'Predicates that require a file or directory to exist throw an error when that is not the case. But the exact exception term is currently backend Prolog compiler dependent.'
		],
		see_also is [os, os_types]
	]).

	:- public(pid/1).
	:- mode(pid(-integer), one).
	:- info(pid/1, [
		comment is 'Returns the process identifier of the running process.',
		argnames is ['PID']
	]).

	:- public(shell/2).
	:- mode(shell(+atom, -integer), one).
	:- info(shell/2, [
		comment is 'Runs an operating-system shell command and returns its exit status.',
		argnames is ['Command', 'Status']
	]).

	:- public(shell/1).
	:- mode(shell(+atom), zero_or_one).
	:- info(shell/1, [
		comment is 'Runs an operating-system shell command.',
		argnames is ['Command']
	]).

	:- public(is_absolute_file_name/1).
	:- mode(is_absolute_file_name(+atom), zero_or_one).
	:- info(is_absolute_file_name/1, [
		comment is 'True iff the argument is an absolute file path. On POSIX systems, this predicate is true if ``File`` starts with a ``/``. On Windows systems, this predicate is true if ``File`` starts with a drive letter. No attempt is made to expand ``File`` as a path.',
		argnames is ['File']
	]).

	:- public(absolute_file_name/2).
	:- mode(absolute_file_name(+atom, -atom), one).
	:- info(absolute_file_name/2, [
		comment is 'Expands a file name to an absolute file path. An environment variable at the beginning of the file name is also expanded.',
		argnames is ['File', 'Path']
	]).

	:- public(decompose_file_name/3).
	:- mode(decompose_file_name(+atom, ?atom, ?atom), one).
	:- info(decompose_file_name/3, [
		comment is 'Decomposes a file name into its directory (which always ends with a slash; ``./`` is returned if absent) and its basename (which can be the empty atom).',
		argnames is ['File', 'Directory', 'Basename']
	]).

	:- public(decompose_file_name/4).
	:- mode(decompose_file_name(+atom, ?atom, ?atom, ?atom), one).
	:- info(decompose_file_name/4, [
		comment is 'Decomposes a file name into its directory (which always ends with a slash; ``./`` is returned if absent), name (that can be the empty atom), and extension (which starts with a ``.`` when defined; the empty atom otherwise).',
		argnames is ['File', 'Directory', 'Name', 'Extension']
	]).

	:- public(path_concat/3).
	:- mode(path_concat(+atom, +atom, --atom), one).
	:- info(path_concat/3, [
		comment is 'Concatenates a path prefix and a path suffix, adding  a ``/`` separator if required. Returns ``Suffix`` when it is an absolute path. Returns ``Prefix`` with a trailing ``/`` appended if missing when ``Suffix`` is the empty atom.',
		argnames is ['Prefix', 'Suffix', 'Path']
	]).

	:- public(internal_os_path/2).
	:- mode(internal_os_path(+atom, -atom), one).
	:- mode(internal_os_path(-atom, +atom), one).
	:- info(internal_os_path/2, [
		comment is 'Converts between the internal path representation (which is backend dependent) and the operating-system native path representation.',
		argnames is ['InternalPath', 'OSPath']
	]).

	:- public(make_directory/1).
	:- mode(make_directory(+atom), one).
	:- info(make_directory/1, [
		comment is 'Makes a new directory. Succeeds if the directory already exists.',
		argnames is ['Directory']
	]).

	:- public(make_directory_path/1).
	:- mode(make_directory_path(+atom), one).
	:- info(make_directory_path/1, [
		comment is 'Makes a new directory creating all the intermediate directories if necessary. Succeeds if the directory already exists.',
		argnames is ['Directory']
	]).

	:- public(delete_directory/1).
	:- mode(delete_directory(+atom), one_or_error).
	:- info(delete_directory/1, [
		comment is 'Deletes an empty directory. Throws an error if the directory does not exist.',
		argnames is ['Directory']
	]).

	:- public(change_directory/1).
	:- mode(change_directory(+atom), one_or_error).
	:- info(change_directory/1, [
		comment is 'Changes current working directory. Throws an error if the directory does not exist.',
		argnames is ['Directory']
	]).

	:- public(working_directory/1).
	:- mode(working_directory(?atom), zero_or_one).
	:- info(working_directory/1, [
		comment is 'Current working directory.',
		argnames is ['Directory']
	]).

	:- public(temporary_directory/1).
	:- mode(temporary_directory(?atom), one).
	:- info(temporary_directory/1, [
		comment is 'Temporary directory. Tries first environment variables: ``TEMP`` and ``TMP`` on Windows systems; ``TMPDIR``, ``TMP``, ``TEMP``, and ``TEMPDIR`` on POSIX systems. When not defined, tries default locations. Returns the working directory as last resort.',
		argnames is ['Directory']
	]).

	:- public(null_device_path/1).
	:- mode(null_device_path(?atom), one).
	:- info(null_device_path/1, [
		comment is 'Null device path: ``nul`` on Windows systems and ``/dev/null`` on POSIX systems.',
		argnames is ['Path']
	]).

	:- public(full_device_path/1).
	:- mode(full_device_path(?atom), zero_or_one).
	:- info(full_device_path/1, [
		comment is 'Full device path: ``/dev/full`` on Linux and BSD systems. Fails on other systems. Experimental.',
		argnames is ['Path']
	]).

	:- public(directory_files/2).
	:- mode(directory_files(+atom, -list(atom)), one_or_error).
	:- info(directory_files/2, [
		comment is 'Returns a list of all files (including directories, regular files, and hidden directories and files) in a directory. File paths are relative to the directory. Throws an error if the directory does not exist.',
		argnames is ['Directory', 'Files']
	]).

	:- public(directory_files/3).
	:- mode(directory_files(+atom, -list(atom), +list(compound)), one_or_error).
	:- info(directory_files/3, [
		comment is 'Returns a list of files filtered using the given list of options. Invalid options are ignored. Default option values are equivalent to ``directory_files/2``. Throws an error if the directory does not exist.',
		argnames is ['Directory', 'Files', 'Options'],
		remarks is [
			'Option ``paths/1``' - 'Possible values are ``relative`` and ``absolute``. Default is ``relative``.',
			'Option ``type/1``' - 'Possible values are ``all``, ``regular``, ``directory``. Default is ``all``.',
			'Option ``extensions/1``' - 'Argument is a list of required extensions (using the format ``''.ext''``). Default is the empty list.',
			'Option ``prefixes/1``' - 'Argument is a list of required file prefixes (atoms). Default is the empty list.',
			'Option ``suffixes/1``' - 'Argument is a list of required file suffixes (atoms). Default is the empty list.',
			'Option ``dot_files/1``' - 'Possible values are ``true`` and ``false``. Default is ``true``.'
		]
	]).

	:- public(directory_exists/1).
	:- mode(directory_exists(+atom), zero_or_one).
	:- info(directory_exists/1, [
		comment is 'True if the specified directory exists (irrespective of directory permissions).',
		argnames is ['Directory']
	]).

	:- public(ensure_directory/1).
	:- mode(ensure_directory(+atom), one).
	:- info(ensure_directory/1, [
		comment is 'Ensures that a directory exists, creating it if necessary.',
		argnames is ['Directory']
	]).

	:- public(file_exists/1).
	:- mode(file_exists(+atom), zero_or_one).
	:- info(file_exists/1, [
		comment is 'True if the specified file exists and is a regular file (irrespective of file permissions).',
		argnames is ['File']
	]).

	:- public(file_modification_time/2).
	:- mode(file_modification_time(+atom, -integer), one_or_error).
	:- info(file_modification_time/2, [
		comment is 'File modification time (which can be used for comparison). Throws an error if the file does not exist.',
		argnames is ['File', 'Time']
	]).

	:- public(file_size/2).
	:- mode(file_size(+atom, -integer), one_or_error).
	:- info(file_size/2, [
		comment is 'File size (in bytes). Throws an error if the file does not exist.',
		argnames is ['File', 'Size']
	]).

	:- public(file_permission/2).
	:- mode(file_permission(+atom, +atom), zero_or_one_or_error).
	:- info(file_permission/2, [
		comment is 'True iff the specified file has the specified permission (``read``, ``write``, or ``execute``). Throws an error if the file does not exist.',
		argnames is ['File', 'Permission']
	]).

	:- public(copy_file/2).
	:- mode(copy_file(+atom, +atom), one_or_error).
	:- info(copy_file/2, [
		comment is 'Copies a file. Throws an error if the original file does not exist or if the copy cannot be created.',
		argnames is ['File', 'Copy']
	]).

	:- public(rename_file/2).
	:- mode(rename_file(+atom, +atom), one_or_error).
	:- info(rename_file/2, [
		comment is 'Renames a file or a directory. Throws an error if the file or directory does not exist.',
		argnames is ['Old', 'New']
	]).

	:- public(delete_file/1).
	:- mode(delete_file(+atom), one_or_error).
	:- info(delete_file/1, [
		comment is 'Deletes a file. Throws an error if the file does not exist.',
		argnames is ['File']
	]).

	:- public(ensure_file/1).
	:- mode(ensure_file(+atom), one).
	:- info(ensure_file/1, [
		comment is 'Ensures that a file exists, creating it if necessary.',
		argnames is ['File']
	]).

	:- public(environment_variable/2).
	:- mode(environment_variable(+atom, ?atom), zero_or_one).
	:- info(environment_variable/2, [
		comment is 'Returns an environment variable value. Fails if the variable does not exists.',
		argnames is ['Variable', 'Value']
	]).

	:- public(time_stamp/1).
	:- mode(time_stamp(-ground), one).
	:- info(time_stamp/1, [
		comment is 'Returns a system-dependent time stamp, which can be used for sorting, but should be regarded otherwise as an opaque term.',
		argnames is ['Time']
	]).

	:- public(date_time/7).
	:- mode(date_time(-integer, -integer, -integer, -integer, -integer, -integer, -integer), one).
	:- info(date_time/7, [
		comment is 'Returns the current date and time. Note that most backends do not provide sub-second accuracy and in those cases the value of the ``Milliseconds`` argument is always zero.',
		argnames is ['Year', 'Month', 'Day', 'Hours', 'Minutes', 'Seconds', 'Milliseconds']
	]).

	:- public(cpu_time/1).
	:- mode(cpu_time(-number), one).
	:- info(cpu_time/1, [
		comment is 'System cpu time in seconds.',
		argnames is ['Seconds']
	]).

	:- public(wall_time/1).
	:- mode(wall_time(-number), one).
	:- info(wall_time/1, [
		comment is 'Wall time in seconds.',
		argnames is ['Seconds']
	]).

	:- public(operating_system_type/1).
	:- mode(operating_system_type(?atom), one).
	:- info(operating_system_type/1, [
		comment is 'Operating system type. Possible values are ``unix``, ``windows``, and ``unknown``.',
		argnames is ['Type']
	]).

	:- public(command_line_arguments/1).
	:- mode(command_line_arguments(-list(atom)), one).
	:- info(command_line_arguments/1, [
		comment is 'Returns a list with the command line arguments that occur after ``--``.',
		argnames is ['Arguments']
	]).

	:- public(sleep/1).
	:- mode(sleep(+number), one).
	:- info(sleep/1, [
		comment is 'Suspends execution the given number of seconds.',
		argnames is ['Seconds']
	]).

:- end_protocol.
