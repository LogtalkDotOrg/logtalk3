%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the examples
% found on the ECLiPSe 5.10#141 documentation (August 2008)


% the constraint solver libraries must always be loaded prior to compilation of 
% the individual example files:
:- lib(ic).


:- object(puzzle).

	:- public([sendmore1/1, sendmore2/1]).

    % we must define an alias (ins/2) for the ECLiPSe "ic" library operator ::/2
    % in order to avoid conflicts with the ::/2 Logtalk message sending operator
	% ECLiPSE 6.0#78 adds an alias in_set_range/2 for ::/2 that could also be used
	:- use_module(ic, [alldifferent/1, (::)/2:ins/2, labeling/1, (#=)/2, (#\=)/2]).
    :- op(700, xfx, ins).

    sendmore1(Digits) :-
        Digits = [S,E,N,D,M,O,R,Y],
        Digits ins [0..9],
        alldifferent(Digits),
        S #\= 0,
        M #\= 0,
                     1000*S + 100*E + 10*N + D
                   + 1000*M + 100*O + 10*R + E
        #= 10000*M + 1000*O + 100*N + 10*E + Y,
        labeling(Digits).

    sendmore2(Digits) :-			% different model, with carries
        Digits = [S,E,N,D,M,O,R,Y],
        Digits ins [0..9],
        Carries = [C1,C2,C3,C4],
        Carries ins [0..1],
        alldifferent(Digits),
        S #\= 0,
        M #\= 0,
        C1         #= M,
        C2 + S + M #= O + 10*C1,
        C3 + E + O #= N + 10*C2,
        C4 + N + R #= E + 10*C3,
             D + E #= Y + 10*C4,
        labeling(Carries),
        labeling(Digits).

:- end_object.
