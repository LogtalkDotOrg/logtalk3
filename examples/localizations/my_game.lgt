%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(my_game(_)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/08/23,
		comment is 'A simple example of supporting application localization in multiple languages.',
		parnames is ['CountryCode']
	]).

	% we use an object parameter to pass the country code
	% of the language to be used when printing messages

	:- public(banner/0).

	banner :-
		parameter(1, CountryCode),
		logtalk::print_message(comment, my_game(CountryCode), banner).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).
	logtalk::message_prefix_stream(comment, my_game(_), '>>> ', user_output).

:- end_object.
