%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
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
%  Public License 3. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization(logtalk_load(profilerp, [reload(skip)])).


:- if(current_logtalk_flag(prolog_dialect, yap)).

	:- initialization(logtalk_load(yap_profiler, [reload(skip)])).

:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	:- initialization(logtalk_load(sicstus_profiler, [reload(skip)])).

:- elif(current_logtalk_flag(prolog_dialect, swi)).

	:- initialization((
		nl,
		write('Logtalk natively supports the SWI-Prolog XPCE profiler.'),
		nl
	)).

:- else.

	:- initialization((
		nl,
		write('Your back-end Prolog compiler does not support a suitable profiler.'),
		nl
	)).

:- endif.
