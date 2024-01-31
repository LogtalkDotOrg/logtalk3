%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2020-2021 Paulo Moura   <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2012 Mauro Ferrari      <mauro.ferrari@uninsubria.it>
%  SPDX-FileCopyrightText: 2012 Camillo Fiorentini <fiorenti@dsi.unimi.it>
%  SPDX-FileCopyrightText: 2012 Guido Fiorino      <guido.fiorino@unimib.it>
%  SPDX-License-Identifier: GPL-2.0-or-later
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


:- op(1200, xfy, <=>).
:- op(1110, xfy, =>).
:- op(1100, xfy, v).
:- op(1000, xfy, &&).
:- op(500, fy, ~).

:- if(current_logtalk_flag(prolog_dialect, lvm)).
	abort :-
		throw(abort).
:- endif.

:- initialization((
	logtalk_load(basic_types(loader)),
	logtalk_load(sets(loader)),
	logtalk_load(os(loader)),
	logtalk_load(fcube, [disjunctions(silent), optimize(on)])
)).
