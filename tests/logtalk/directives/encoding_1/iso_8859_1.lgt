% the next directive should trigger a misplaced warning
:- encoding('ISO-8859-1').

% the next directive should be ignored by the compiler,
% which, by default, should result in a printed warning
:- encoding('UTF-8').
