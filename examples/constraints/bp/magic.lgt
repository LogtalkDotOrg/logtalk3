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
% distributed with B-Prolog 7.1 (August 2008)


:- object(magic).

	:- public(go/0).

    go:-
        statistics(runtime,[Start|_]),
        top,
        statistics(runtime,[End|_]),
        T is End-Start,
        write('execution time is '),write(T), write(milliseconds),nl.

    top:-
        magicSquare(7).

    magicSquare(N) :- 
        new_array(Matrix,[N,N]),
        NN is N*N,
        term_variables(Matrix,Vars),
        Vars in 1..NN,
        Sum is NN*(NN+1)//(2*N),
        {Matrix^rows @=Rows},     % (@=)/2 is not declared built-in...
        sumRows(Rows,Sum),
        {Matrix^columns @=Cols},
        sumRows(Cols,Sum),
        {Matrix^diagonal1 @= Diag1},
        sum(Diag1) #= Sum,
        {Matrix^diagonal2 @= Diag2},
        sum(Diag2) #= Sum,
        all_different(Vars),
        labeling_ffc(Vars),
        writeln(Rows).

    sumRows([],_).
    sumRows([Row|Rows],Sum):-
        sum(Row) #= Sum,
        sumRows(Rows,Sum).

:- end_object.
