%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.21,
		author is 'Paulo Moura',
		date is 2017/09/10,
		comment is 'Portable operating-system access protocol.',
		see_also is [os_types]
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

	:- public(absolute_file_name/2).
	:- mode(absolute_file_name(+atom, -atom), one).
	:- info(absolute_file_name/2, [
		comment is 'Expands a file name to an absolute file path. An environment variable at the beginning of the file name is also expanded.',
		argnames is ['File', 'Path']
	]).

	:- public(decompose_file_name/3).
	:- mode(decompose_file_name(+atom, ?atom, ?atom), one).
	:- info(decompose_file_name/3, [
		comment is 'Decomposes a file name into its directory (which always ends with a slash; "./" is returned if absent) and its basename (that can be the empty atom).',
		argnames is ['File', 'Directory', 'Basename']
	]).

	:- public(decompose_file_name/4).
	:- mode(decompose_file_name(+atom, ?atom, ?atom, ?atom), one).
	:- info(decompose_file_name/4, [
		comment is 'Decomposes a file name into its directory (which always ends with a slash; "./" is returned if absent), name (that can be the empty atom), and extension (which starts with a "." when defined; the empty atom otherwise).',
		argnames is ['File', 'Directory', 'Name', 'Extension']
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
	:- mode(delete_directory(+atom), one).
	:- info(delete_directory/1, [
		comment is 'Deletes an empty directory.',
		argnames is ['Directory']
	]).

	:- public(change_directory/1).
	:- mode(change_directory(+atom), one).
	:- info(change_directory/1, [
		comment is 'Changes current working directory.',
		argnames is ['Directory']
	]).

	:- public(working_directory/1).
	:- mode(working_directory(?atom), zero_or_one).
	:- info(working_directory/1, [
		comment is 'Current working directory.',
		argnames is ['Directory']
	]).

	:- public(directory_files/2).
	:- mode(directory_files(+atom, -list(atom)), one).
	:- info(directory_files/2, [
		comment is 'Returns a list of all files (including directories, regular files, and hidden directories and files) in a directory. File paths are relative to the directory.',
		argnames is ['Directory', 'Files']
	]).

	:- public(directory_files/3).
	:- mode(directory_files(+atom, -list(atom), +list(compound)), one).
	:- info(directory_files/3, [
		comment is 'Returns a filtered list of directory files. Filter options are: paths/1 - {relative,absolute}, type/1 - {all,regular,directory}, extensions/1 - list, and dot_files/1 - boolean. Invalid options are ignored. Default options equivalent to directory_files/2.',
		argnames is ['Directory', 'Files', 'Options']
	]).

	:- public(directory_exists/1).
	:- mode(directory_exists(+atom), zero_or_one).
	:- info(directory_exists/1, [
		comment is 'True if the specified directory exists (irrespective of directory permissions).',
		argnames is ['Directory']
	]).

	:- public(file_exists/1).
	:- mode(file_exists(+atom), zero_or_one).
	:- info(file_exists/1, [
		comment is 'True if the specified file exists and is a regular file (irrespective of file permissions).',
		argnames is ['File']
	]).

	:- public(file_modification_time/2).
	:- mode(file_modification_time(+atom, -integer), zero_or_one).
	:- info(file_modification_time/2, [
		comment is 'File modification time (which can be used for comparison).',
		argnames is ['File', 'Time']
	]).

	:- public(file_size/2).
	:- mode(file_size(+atom, -integer), zero_or_one).
	:- info(file_size/2, [
		comment is 'File size (in bytes).',
		argnames is ['File', 'Size']
	]).

	:- public(file_permission/2).
	:- mode(file_permission(+atom, ?atom), zero_or_one).
	:- info(file_permission/2, [
		comment is 'True if the specified file has the specified permission (read, write, or execute).',
		argnames is ['File', 'Permission']
	]).

	:- public(rename_file/2).
	:- mode(rename_file(+atom, +atom), zero_or_one).
	:- info(rename_file/2, [
		comment is 'Renames a file or a directory.',
		argnames is ['Old', 'New']
	]).

	:- public(delete_file/1).
	:- mode(delete_file(+atom), one).
	:- info(delete_file/1, [
		comment is 'Deletes a file.',
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
		comment is 'Returns the current date and time.',
		argnames is ['Year', 'Month', 'Day', 'Hours', 'Minutes', 'Seconds', 'Miliseconds']
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
		comment is 'Operating system type. Possible values are "unix", "windows", and "unknown".',
		argnames is ['Type']
	]).

	:- public(command_line_arguments/1).
	:- mode(command_line_arguments(-list(atom)), one).
	:- info(command_line_arguments/1, [
		comment is 'Returns a list with the command line arguments that occur after "--".',
		argnames is ['Arguments']
	]).

:- end_protocol.
