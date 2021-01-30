%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2020-2021 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_logtalk_flag(hook, toychrdb).


:- object(dom,
	extends(toychrdb)).

	dom(_, []) <=> fail.
	dom(N, [M]) <=> N = M.
	dom(N, Domain) <=>
		nonvar(N) | list::memberchk(N, Domain).
	dom(N, Domain1), dom(N, Domain2) <=>
		sort(Domain1, Set1), sort(Domain2, Set2),
		set::intersection(Set1, Set2, Set), dom(N, Set).

:- end_object.