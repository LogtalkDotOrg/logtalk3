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
a(0).

:- if(true).
	a(1).
:- endif.

:- if(fail).
	a(2).
:- endif.

% tests of if/1...else/0...endif/0
b(0).

:- if(true).
	b(1).
:- else.
	b(2).
:- endif.

:- if(fail).
	b(3).
:- else.
	b(4).
:- endif.

% tests of if/1...elif/1...else/0...endif/0
c(0).

:- if(true).
	c(1).
:- elif(true).
	c(2).
:- else.
	c(3).
:- endif.

c(4).

:- if(fail).
	c(5).
:- elif(true).
	c(6).
:- else.
	c(7).
:- endif.

c(8).

:- if(fail).
	c(9).
:- elif(fail).
	c(10).
:- else.
	c(11).
:- endif.

c(12).

:- if(fail).
	c(13).
:- elif(fail).
	c(14).
:- elif(true).
	c(15).
:- else.
	c(16).
:- endif.

% tests of if/1...if/1...endif/0
d(0).

:- if(true).

	d(1).
	:- if(true).
		d(2).
	:- endif.

	d(3).
	:- if(fail).
		d(4).
	:- endif.

	d(5).
	:- if(true).
		d(6).
	:- else.
		d(7).
	:- endif.

	d(8).
	:- if(fail).
		d(9).
	:- else.
		d(10).
	:- endif.

	d(11).

:- else.

	d(12).

:- endif.

% tests of if/1...elif/1...endif/0
e(0).

:- if(fail).

	e(1).
	:- if(true).
		e(2).
	:- endif.

	e(3).
	:- if(fail).
		e(4).
	:- else.
		e(5).
	:- endif.

	e(6).
	:- if(fail).
		e(7).
	:- elif(true).
		e(8).
	:- else.
		e(9).
	:- endif.

	e(10).

:- elif(fail).

	e(11).

:- else.

	e(12).

:- endif.

z(0).
