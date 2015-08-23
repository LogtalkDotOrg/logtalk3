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
	% load the application core logic code
	logtalk_load(my_game),
	% load all the application localizations;
	% tipically we would load just a single one
	% based on some setting
	logtalk_load(my_game_de_localization),
	logtalk_load(my_game_en_localization),
	logtalk_load(my_game_fr_localization),
	logtalk_load(my_game_pt_localization)
)).
