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

% tests of if/1...if/1...endif/0
dd(0).

:- if(true).

	dd(1).
	:- if(true).
		dd(2).
	:- endif.

	dd(3).
	:- if(fail).
		dd(4).
	:- endif.

	dd(5).
	:- if(true).
		dd(6).
	:- else.
		dd(7).
	:- endif.

	dd(8).
	:- if(fail).
		dd(9).
	:- else.
		dd(10).
	:- endif.

	dd(11).

:- else.

	dd(12).

:- endif.

% tests of if/1...elif/1...endif/0
ee(0).

:- if(fail).

	ee(1).
	:- if(true).
		ee(2).
	:- endif.

	ee(3).
	:- if(fail).
		ee(4).
	:- else.
		ee(5).
	:- endif.

	ee(6).
	:- if(fail).
		ee(7).
	:- elif(true).
		ee(8).
	:- else.
		ee(9).
	:- endif.

	ee(10).

:- elif(fail).

	ee(11).

:- else.

	ee(12).

:- endif.

zz(0).
