%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(metaclass,			% avoid infinite metaclass regression by
	instantiates(metaclass)).	% making the class its own metaclass

:- end_object.


:- object(class,
	instantiates(metaclass)).

	:- public(p1/1).

	p1(class).

:- end_object.


:- object(instance,
	instantiates(class)).

:- end_object.
