%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  sample tester file
%  Last updated on December 25, 2014
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%  This is a sample tester file. Tester files are simply Logtalk source files
%  whose main purpose is to load and run your application unit tests.
%
%  The provided testing automation shell script, "logtalk_tester", looks for
%  files named "tester.lgt" in directories and sub-directories when run.


:- initialization((
	% minimize compilation reports to the essential ones
	set_logtalk_flag(report, warnings),
	% load any necessary library files; for example:
	% logtalk_load(library(basic_types_loader)),
	% load the unit test tool
	logtalk_load(lgtunit(loader)),
	% load your application files (e.g. "source.lgt") enabling supporting for
	% code coverage, which requires compilation in debug mode and collecting
	% source data information; if code coverage is not wanted, simply remove
	% the second argument for faster execution
	logtalk_load(source, [source_data(on), debug(on)]),
	% compile the unit tests file expanding it using the "lgtunit" object
	% to preprocess the tests
	logtalk_load(tests, [hook(lgtunit)]),
	% run all the unit tests
	tests::run
)).
