%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((
	current_logtalk_flag(version, version(Generation, Release, Update)),
	write('*****         Logtalk version: '), write(Generation), write('.'), write(Release), write('.'), write(Update), nl,
	current_logtalk_flag(prolog_version, (Major, Minor, Patch)),
	write('*****         Prolog version: '), write(Major), write('.'), write(Minor), write('.'), write(Patch), nl
)).
