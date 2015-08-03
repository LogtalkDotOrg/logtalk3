%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% tests of if/1...endif/0
aa(0).

:- if(true).
	aa(1).
:- endif.

:- if(fail).
	aa(2).
:- endif.

% tests of if/1...else/0...endif/0
bb(0).

:- if(true).
	bb(1).
:- else.
	bb(2).
:- endif.

:- if(fail).
	bb(3).
:- else.
	bb(4).
:- endif.

% tests of if/1...elif/1...else/0...endif/0
cc(0).

:- if(true).
	cc(1).
:- elif(true).
	cc(2).
:- else.
	cc(3).
:- endif.

cc(4).

:- if(fail).
	cc(5).
:- elif(true).
	cc(6).
:- else.
	cc(7).
:- endif.

cc(8).

:- if(fail).
	cc(9).
:- elif(fail).
	cc(10).
:- else.
	cc(11).
:- endif.

cc(12).

:- if(fail).
	cc(13).
:- elif(fail).
	cc(14).
:- elif(true).
	cc(15).
:- else.
	cc(16).
:- endif.


:- if(true).
	dd(1).
:- elif(fail).
	dd(2).
:- elif(fail).
	dd(3).
:- else.
	dd(4).
:- endif.


:- if(fail).
	ee(5).
:- elif(fail).
	ee(6).
:- elif(fail).
	ee(7).
:- else.
	ee(8).
:- endif.


% tests for embedded ifs
:- if(true).
	ff(1).
:- elif(fail).
	ff(2).
	:- if(true).
		ff(3).
	:- elif(true).
		ff(4).
	:- else.
		ff(5).
	:- endif.
:- elif(fail).
	ff(6).
:- else.
	ff(7).
:- endif.


:- if(true).
	gg(1).
	:- if(true).
		gg(2).
	:- elif(true).
		gg(3).
	:- else.
		gg(4).
	:- endif.
:- elif(fail).
	gg(5).
:- elif(fail).
	gg(6).
:- else.
	gg(7).
:- endif.


:- if(fail).
	hh(1).
:- elif(fail).
	hh(2).
	:- if(true).
		hh(3).
	:- elif(true).
		hh(4).
	:- else.
		hh(5).
	:- endif.
:- elif(fail).
	hh(6).
:- else.
	hh(7).
:- endif.


:- if(fail).
	ii(1).
:- elif(true).
	ii(2).
:- else.
	ii(3).
	:- if(fail).
		ii(4).
	:- elif(fail).
		ii(5).
	:- elif(fail).
		ii(6).
	:- endif.
	ii(7).
:- endif.


% tests of if/1...if/1...endif/0
jj(0).

:- if(true).

	jj(1).
	:- if(true).
		jj(2).
	:- endif.

	jj(3).
	:- if(fail).
		jj(4).
	:- endif.

	jj(5).
	:- if(true).
		jj(6).
	:- else.
		jj(7).
	:- endif.

	jj(8).
	:- if(fail).
		jj(9).
	:- else.
		jj(10).
	:- endif.

	jj(11).

:- else.

	jj(12).

:- endif.

% tests of if/1...elif/1...endif/0
ll(0).

:- if(fail).

	ll(1).
	:- if(true).
		ll(2).
	:- endif.

	ll(3).
	:- if(fail).
		ll(4).
	:- else.
		ll(5).
	:- endif.

	ll(6).
	:- if(fail).
		ll(7).
	:- elif(true).
		ll(8).
	:- else.
		ll(9).
	:- endif.

	ll(10).

:- elif(fail).

	ll(11).

:- else.

	ll(12).

:- endif.

zz(0).
