%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  sample loader file
%  Last updated on October 29, 2013
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


%  This is a sample loader file. Loader files are simply Logtalk source
%  files whose main purpose is to load your application files.
%
%  During development, loader files can be reloaded repeatedly. Therefore,
%  generic settings shared by several applications, such as library paths
%  and global flag values should be done preferably using a settings file.


%  Is handy to be able to reload a loader file even if not modified in
%  order to reload any modified application source files.  This can be
%  ensured, independent of global vaue of the `reload` flag, using the
%  following directive (whose scope is local to this file):

:- set_logtalk_flag(reload, always).


%  Load your application files:

:- initialization((
	logtalk_load([
		source_file_1,
		source_file_2,
		...
	])
)).
