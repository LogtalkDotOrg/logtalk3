%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
:- if(true).
	s1.
:- endif.


:- if(fail).
	f1.
:- endif.


:- if(true).
	s2.
:- elif(fail).
	f2.
:- elif(fail).
	f3.
:- else.
	f4.
:- endif.


:- if(fail).
	f5.
:- elif(fail).
	f6.
:- elif(fail).
	f7.
:- else.
	s3.
:- endif.

:- if(true).
	s4.
:- elif(fail).
	f8.
	:- if(true).
		f9.
	:- elif(true).
		f10.
	:- else.
		f11.
	:- endif.
:- elif(fail).
	f12.
:- else.
	f14.
:- endif.


:- if(fail).
	f15.
:- elif(fail).
	f16.
	:- if(true).
		f17.
	:- elif(true).
		f18.
	:- else.
		f19.
	:- endif.
:- elif(fail).
	f29.
:- else.
	s5.
:- endif.
*/

:- if(true).
	f15.
	:- if(true).
		f17.
	:- elif(true).
		f18.
	:- else.
		f19.
	:- endif.
:- elif(fail).
	f16.
:- elif(fail).
	f29.
:- else.
	s5.
:- endif.
