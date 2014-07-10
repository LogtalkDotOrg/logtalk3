%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  Integration file for SWI-Prolog
%  Last updated on July 10, 2014
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


% load Logtalk core files
:-	getenv('LOGTALKHOME', LogtalkHome0),
	prolog_to_os_filename(LogtalkHome, LogtalkHome0),
	atom_concat(LogtalkHome, '/adapters/swi.pl', AdapterFile), consult(AdapterFile),
	atom_concat(LogtalkHome, '/paths/paths.pl', PathsFile), consult(PathsFile),
	atom_concat(LogtalkHome, '/integration/logtalk_comp_swi.pl', IntegrationFile), consult(IntegrationFile),
	atom_concat(LogtalkHome, '/adapters/swihooks.pl', HooksFile), consult(HooksFile).
