%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2012 Mauro Ferrari      <mauro.ferrari@uninsubria.it>
%  Copyright 2012 Camillo Fiorentini <fiorenti@dsi.unimi.it>
%  Copyright 2012 Guido Fiorino      <guido.fiorino@unimib.it>
%  Copyright 2020-2021 Paulo Moura   <pmoura@logtalk.org>
%  SPDX-License-Identifier: GPL-2.0-or-later
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(fcube).

	:- info([
		version is 4:1:1,
		author is 'Mauro Ferrari, Camillo Fiorentini, Guido Fiorino; ported to Logtalk by Paulo Moura.',
		date is 2022-09-07,
		copyright is 'Copright 2012 Mauro Ferrari, Camillo Fiorentini, Guido Fiorino; Copyright 2022 Paulo Moura',
		license is 'GNU GPL 2.0 or later version',
		comment is 'FCube: An Efficient Prover for Intuitionistic Propositional Logic.'
	]).

	:- public([
		fcube/0, decide/1, decide/2
	]).

	:- public([
		op(1200, xfy, <=>),
		op(1110, xfy, =>),
		op(1000, xfy, &),
		op(500, fy, ~)
	]).

	:- if((\+ current_logtalk_flag(prolog_dialect, xsb), \+ current_logtalk_flag(prolog_dialect, ji), \+ current_op(_, _, '|'))).
		:- public(op(1100, xfy, '|')).
	:- endif.

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	:- uses(os, [
		cpu_time/1
	]).

	:- uses(set, [
		as_set/2 as list_to_set/2, intersection/3, subtract/3, union/3
	]).

	:- uses(user, [
		abort/0, atomic_concat/3
	]).

	/****************************

	Ottenuto da fCube-verbose-4.0

	****************************/

	gnu :-
		writeln('This is Fcube\nCopyright (C) 2012'),
		writeln('Mauro Ferrari,      email: mauro.ferrari@uninsubria.it,'),
		writeln('Camillo Fiorentini, email: fiorenti@dsi.unimi.it'),
		writeln('Guido Fiorino,      email: guido.fiorino@unimib.it'),
		writeln('This program comes with ABSOLUTELY NO WARRANTY.'),
		writeln('This is free software, and you are welcome to redistribute it'),
		writeln('under  conditions  GNU  Public License, see <http://www.gnu.org/licenses/> for details.').

	fcube:-
		 read(X),
		 decide(X).

	decide(X):-
		intDecide(X, _, 1).

	% added to support tests
	decide(X, COUNTERMODEL):-
		intDecide(X, COUNTERMODEL, 1).

	intDecide(X, COUNTERMODEL, NUMERO):-
		writeln('Input Formula:'),
		printSWFF(swff(f, X)),
		writeln('\n'),
		permanenzaSegno([swff(f, X)], StartingSet),
		writeln('Sign Permanence tried,input formula equivalent to: '),
		printSWFFSet(StartingSet, 1),
		orderEquivSet(StartingSet, OrderedStartingSet),
		time(reapply(OrderedStartingSet, COUNTERMODEL, 1, 1)),
		!,
		writeln(' '),
		write(NUMERO),
		writeln(' search result = unprovable (fCube-4.1)'),
		writeln('*** The Counter Model (see also the prolog term ) ***'),
		writeln('-- root --'),
		printKripke(COUNTERMODEL, 1),
		writeln('-- end world --'),
		writeln('*** Prolog term of the countermodel ***'),
		writeln(COUNTERMODEL).

	intDecide(_, [valida], NUMERO):-
		writeln(' '),
		write(NUMERO),
		writeln(' search result = provable (fCube-4.1)').

	/*
	applica la semplificazione di Massacci, con A lista dei candidati, cioÃ¨ le formule appena ottenute
	per applicazione della regola
	*/
	semplificazioneCerta(A, B, NewerSet) :-
		simplification(A, B, NewSet),
		!,
		(
			coerente(NewSet),
			permanenzaSegno(NewSet, NewerSet),
			!
		;
			NewerSet = NewSet
		).

	semplificazioneBreve(A, B, NewSet) :-
		simplification(A, B, NewSet),
		!.


	semplificazioneBranch(A, B, LastSet) :-
		semplificazione(A, B, LastSet).

	semplificazione(A, B, TheMoreNewSet) :-
		simplification(A, B, NewSet),
		!,
		(
			coerente(NewSet),
			permanenzaSegno(NewSet, NewerSet),
			!,
			(
				coerente(NewerSet),
				!,
				clSimplification(NewerSet, TheMoreNewSet),
				!
			;
				TheMoreNewSet = NewerSet
			)
		;
			TheMoreNewSet = NewSet
		).



	/*
	Dopo la semplificazione di B abbiamo un nuovo insieme, NewSet, che potrebbe avere atomi costanti.
	Si calcola gli atomi con segno costante. Tali atomi, se esistono diventano i nuovi candidati per la
	semplificazione, quindi si procede
	*/
	permanenzaSegno(NewSet, NewerSet) :- atomiConSegnoCostanteInSwffSet(NewSet, ListaAtomiConSegnoCostante),
		!,
		iteraSemplificazione(ListaAtomiConSegnoCostante, NewSet, NewerSet),
		!.

	/* se non ci sono candidati, allora non c'Ã¨ nulla da semplificare */
	iteraSemplificazione([], S, S).

	/*
	I candidati in [A|L] vengono concatenati all'insieme ottenuto dalla semplificazione di Massacci.
	Si ottiene TempSet a cui si applica la semplificazione di Massacci, ottenendo NewTempSet.
	A questo punto si puo' iterare il procedimento ricalcolando la permanenza del segno.
	 */
	iteraSemplificazione([A|L], NewSet, NewerSet) :- append([A|L], NewSet, TempSet), !,
		simplification([A|L], TempSet, NewTempSet0), !,
		% Paulo Moura: added the next list_to_set/2 to allow using an ordered set representation
		list_to_set(NewTempSet0, NewTempSet),
		/*chiama permanenza del segno se simplification ha fatto qualche rimpiazzamento*/
		chiamaPermanenzaSegno(TempSet, NewTempSet, NewerSet), !.

	chiamaPermanenzaSegno(TempSet, TempSet, TempSet).
	chiamaPermanenzaSegno(_, NewTempSet, NewerSet) :- permanenzaSegno(NewTempSet, NewerSet), !.

	/*
	NOTA: 1) TRA LE FUNZIONI LEGATE ALLA XMANENZA DEL SEGNO OCCORREREBBE METTERNA UNA CHE FILTRI
	LE SWFF ATOMICHE DOPPIE IN [A|L] CIOÃˆ IN ListaAtomiConSegnoCostante
	*/

	uguali(X, X).

	/*findWFFalpha, data una lista mette, se contiene una formula di tipo alpha
	la mette nella lista a secondo argomento e nel terzo ci mette la prima lista privata della formula alpha.
	*/

	findWFFalpha([], [], []).
	findWFFalpha([H|T], [H], T) :-tipoalpha(H).
	findWFFalpha([H|T], X, [H|Y]) :-findWFFalpha(T, X, Y).


	/* formule di tipo 1*/
	tipoalpha(swff(t, (_ & _))).
	tipoalpha(swff(t, (_ <=> _))).
	tipoalpha(swff(f, (_ | _))).
		/*tipoalpha(swff(t,im(X,_))):-atom(X).*/
		/* valutare se toglierlo e gestire tale formula sempre a parte come caso speciale */
	tipoalpha(swff(t, ((_ & _) => _))).
	tipoalpha(swff(t, ((_ <=> _) => _))).
	tipoalpha(swff(t, ((_ | _) => _))).
	tipoalpha(swff(fc, (_ | _))).
	tipoalpha(swff(t, ~ _)).

	/*formule di tipo 2*/
	tipobeta(swff(f, (_ & _))).
	tipobeta(swff(f, (_ <=> _))).
	tipobeta(swff(t, (_ | _))).


	/*formule di tipo 3*/
	tipo3(swff(f, (_ => _))).
	tipo3(swff(f, ~ _)).

	/*formule tipo4*/
	tipo4(swff(t, ((_ => _) =>_))).
	tipo4(swff(t, (~ _ => _))).

	/*formule tipo5*/
	tipo5(swff(fc, (_ => _))).
	tipo5(swff(fc, ~ _)).

	/*formule tipo6*/
	tipo6(swff(fc, (_ & _))).
	tipo6(swff(fc, (_ <=> _))).


	/****************************************************

		PREDICATI PER LA PERMANENZA DEL SEGNO

	****************************************************/

	/*
	Dato un insieme di swff determina la lista di formule atomiche con segno costante.
	Questo predicato implementa la regola sui segni costanti in un insieme
	*/

	atomiConSegnoCostanteInSwffSet(SwffSet, ListOfAtomicSwffs) :-
		segnoAtomiInSetOfSwff(SwffSet, TempSetOfVarPos, TempSetOfVarNeg),
		!,
		list_to_set(TempSetOfVarPos, SetOfVarPos),
		list_to_set(TempSetOfVarNeg, SetOfVarNeg),
		atomiConSegnoCost(SetOfVarNeg, SetOfVarPos, SwffAtomsWithConstantSign),
		subtract(SwffAtomsWithConstantSign, SwffSet, ListOfAtomicSwffs),
		!.


	/* I SEGUENTI SONO PREDICATI DI SUPPORTO AL PRECEDENTE */

	/*
	Stabilisce il segno degli atomi in un insieme di formule segnate:
	il primo argomento Ã¨ un insieme di swff
	il secondo argomento Ã¨ la lista delle variabili segnate True
	il terzo argomento e' la lista delle variabili  segnate False
	*/


	segnoAtomiInSetOfSwff([], [], []).

	/*
		se la formula in cima alla lista e' atomica
	*/

	segnoAtomiInSetOfSwff([swff(_, 1)|T], PositiveInCoda, NegativeInCoda) :-
		segnoAtomiInSetOfSwff(T, PositiveInCoda, NegativeInCoda).
	segnoAtomiInSetOfSwff([swff(_, 0)|T], PositiveInCoda, NegativeInCoda) :-
		segnoAtomiInSetOfSwff(T, PositiveInCoda, NegativeInCoda).

	segnoAtomiInSetOfSwff([swff(t, X)|T], [X|PositiveInCoda], NegativeInCoda) :-
		atom(X),
		segnoAtomiInSetOfSwff(T, PositiveInCoda, NegativeInCoda).

	segnoAtomiInSetOfSwff([swff(f, X)|T], PositiveInCoda, [X|NegativeInCoda]) :-
		atom(X),
		segnoAtomiInSetOfSwff(T, PositiveInCoda, NegativeInCoda).

	segnoAtomiInSetOfSwff([swff(fc, X)|T], PositiveInCoda, [X|NegativeInCoda]) :-
		atom(X),
		segnoAtomiInSetOfSwff(T, PositiveInCoda, NegativeInCoda).


	segnoAtomiInSetOfSwff([A|T], VarPositive, VarNegative) :-
		segnoAtomiInSwff(A, VarPosInA, VarNegInA),
		segnoAtomiInSetOfSwff(T, VarPosInCoda, VarNegInCoda),
		append(VarPosInA, VarPosInCoda, VarPositive),
		append(VarNegInA, VarNegInCoda, VarNegative).


	segnoAtomiInSwff(swff(t, A), VarPosInA, VarNegInA) :- segnoAtomiInSwffTrue(A, VarPosInA, VarNegInA).
	segnoAtomiInSwff(swff(f, A), VarPosInA, VarNegInA) :- segnoAtomiInSwffFalse(A, VarPosInA, VarNegInA).
	segnoAtomiInSwff(swff(fc, A), VarPosInA, VarNegInA) :- segnoAtomiInSwffFalse(A, VarPosInA, VarNegInA).
	/*
	stabilisce segno degli atomi in una formula segnata.
	Il segno di un atomo puo' essere positivo o negativo
	*/
	/*passo base*/
	segnoAtomiInSwffTrue(X, [X], []) :-atom(X).
	/*passo induttivo*/
	segnoAtomiInSwffTrue((X & Y), Z, Atomi) :- segnoAtomiInSwffTrue(X, L, AtomiX),
		segnoAtomiInSwffTrue(Y, M, AtomiY),
		append(L, M, Z),
		append(AtomiX, AtomiY, Atomi).

	segnoAtomiInSwffTrue((X <=> Y), Z, Z) :- variabili(X, VarX),
		variabili(Y, VarY),
		union(VarX, VarY, Z).

	segnoAtomiInSwffTrue((X | Y), Z, Atomi) :- segnoAtomiInSwffTrue(X, L, AtomiX),
		segnoAtomiInSwffTrue(Y, M, AtomiY),
		append(L, M, Z),
		append(AtomiX, AtomiY, Atomi).

	segnoAtomiInSwffTrue((X => Y), Z, Atomi) :- segnoAtomiInSwffFalse(X, L, AtomiX),
		segnoAtomiInSwffTrue(Y, M, AtomiY),
		append(L, M, Z),
		append(AtomiX, AtomiY, Atomi).

	segnoAtomiInSwffTrue(~X, L, Atomi) :- segnoAtomiInSwffFalse(X, L, Atomi).


	segnoAtomiInSwffFalse(X, [], [X]) :-atom(X).


	segnoAtomiInSwffFalse((X & Y), Z, Atomi) :- segnoAtomiInSwffFalse(X, L, AtomiX),
		segnoAtomiInSwffFalse(Y, M, AtomiY),
		append(L, M, Z),
		append(AtomiX, AtomiY, Atomi).

	segnoAtomiInSwffFalse((X <=> Y), Z, Z) :- variabili(X, VarX),
		variabili(Y, VarY),
		union(VarX, VarY, Z).

	segnoAtomiInSwffFalse((X | Y), Z, Atomi) :- segnoAtomiInSwffFalse(X, L, AtomiX),
		segnoAtomiInSwffFalse(Y, M, AtomiY),
		append(L, M, Z),
		append(AtomiX, AtomiY, Atomi).

	segnoAtomiInSwffFalse((X => Y), Z, Atomi) :- segnoAtomiInSwffTrue(X, L, AtomiX),
		segnoAtomiInSwffFalse(Y, M, AtomiY),
		append(L, M, Z),
		append(AtomiX, AtomiY, Atomi).

	segnoAtomiInSwffFalse(~X, L, Atomi) :- segnoAtomiInSwffTrue(X, L, Atomi).







	/*lista le variabili in una swff*/

	variabili(X, [X]) :- atom(X).

	variabili((X & Y), L) :- variabili(X, M),
		variabili(Y, N),
		append(M, N, L).

	variabili((X <=> Y), L) :-
		variabili(X, M),
		variabili(Y, N),
		append(M, N, L).


	variabili((X | Y), L) :- variabili(X, M),
		variabili(Y, N),
		append(M, N, L).

	variabili((X => Y), L) :- variabili(X, M),
		variabili(Y, N),
		append(M, N, L).

	variabili(~X, L) :- variabili(X, L).


	/*atomiConSegnoCost,stabilisce l'elenco delle variabili che hanno segno costante:
	atomiConSegnoCost(SetOfVarNeg,SetOfVarPos,SwffAtomsWithConstantSign)
	*/

	atomiConSegnoCost(SetOfVarNeg, SetOfVarPos, SwffAtomsWithConstantSign) :-
		subtract(SetOfVarPos, SetOfVarNeg, ConstantVarPos),
		subtract(SetOfVarNeg, SetOfVarPos, ConstantVarNeg),
		buildPositiveAtomic(ConstantVarPos, ConstantPositiveAtomic),
		buildNegativeAtomic(ConstantVarNeg, ConstantNegativeAtomic),
		union(ConstantPositiveAtomic, ConstantNegativeAtomic, SwffAtomsWithConstantSign).

	buildPositiveAtomic([], []).
	buildPositiveAtomic([PropVar|Tail], [swff(t, PropVar)|Res]) :- buildPositiveAtomic(Tail, Res).

	buildNegativeAtomic([], []).
	buildNegativeAtomic([PropVar|Tail], [swff(fc, PropVar)|Res]) :- buildNegativeAtomic(Tail, Res).

	/*
	STAMPE
	printSWFFSet stampa una lista di formule. Il secondo parametro e' l'allineamento
	printSWFF stampa una formula segnata,cioe' un termine che comincia con swff;
	printWFF stampa una formula proposizionale
	*/

	printSWFFSet([], _).

	printSWFFSet([X|Y], SPAZIO) :- !, tab(SPAZIO*3), printSWFF(X), !, writeln(';'), printSWFFSet(Y, SPAZIO), !.

	printSWFF(swff(S, 0)) :-upcase_atom(S, K), write(K), write(' 0').
	printSWFF(swff(S, 1)) :-upcase_atom(S, K), write(K), write(' 1').
	printSWFF(swff(S, X)) :-upcase_atom(S, K), write(K), write(' '), printWFF(X).


	printWFF(X) :-atom(X), write(X).
	printWFF((X => Y)) :-write('('), printWFF(X), !, write(' => '), printWFF(Y), !, write(')').
	printWFF((X | Y)) :-write('('), printWFF(X), !, write(' | '), printWFF(Y), !, write(')').
	printWFF((X & Y)) :-write('('), printWFF(X), !, write('&'), printWFF(Y), !, write(')').
	printWFF((~ X)) :-write('~'), printWFF(X), !.
	printWFF((X <=> Y)) :-write('('), printWFF(X), !, write(' <=> '), printWFF(Y), !, write(')').


	/****************************************************************

	SEMPLIFICAZIONI BOOLEANE DI UNA FORMULA

	valTerm(termine,valore): valuta ricorsivamente un termine:
		se termine Ã¨ atomico allora vale se stesso;
		se termine Ã¨ non atomico viene valutato seguendo la tavola di veritÃ  del connettivo principale.
	************************************************************************************************/

	/*passo base: il valore di un termine atomico Ã¨ se stesso */
	valTerm(0, 0).
	valTerm(1, 1).
	valTerm(X, X) :-atom(X).

	/*passo induttivo: valore di un termine and */
	valTerm((X & Y), RISP) :- valTerm(X, VALX), !,
		andTable(VALX, Y, RISP) /*guarda la tabella di verita' dell'AND*/, !.

	valTerm((X <=> Y), RISP) :- valTerm(X, VALX),
		valTerm(Y, VALY),
		equivTable(VALX, VALY, RISP), /*guarda la tabella di verita' dell'EQUIV*/
		!.

	valTerm((X | Y), RISP) :- valTerm(X, VALX), !, /*valuta il termine di sx*/
		orTable(VALX, Y, RISP) /*guarda la tabella di verita' dell'OR*/, !.

	valTerm((X => Y), RISP) :-valTerm(X, VALX), !, /*valuta il termine di sx*/
		imTable(VALX, Y, RISP) /*guarda la tabella di verita' dell'OR*/, !.

	valTerm(~X, RISP) :-valTerm(X, VALX), !, nonTable(VALX, RISP).

	nonTable(0, 1).
	nonTable(1, 0).
	nonTable(VALX, ~ VALX).


	andTable(0, _, 0). /*se il congiunto di sx vale 0 allora la risposta vale 0 */
	andTable(1, Y, RISP) :-valTerm(Y, RISP), !. /* se il congiunto di sx vale 1 allora la risposta vale il valore del congiunto di dx*/
	andTable(VALX, Y, RISP) :-valTerm(Y, VALY), !, /*se il congiunto di sx Ã¨ un termine qualsiasi allora si valuta il congiunto di dx */
		andTable2(VALX, VALY, RISP). /* e si guarda la tabella dell'and lungo la  colonna del secondo congiunto*/
	andTable2(_, 0, 0). /* se il congiunto di dx vale 0,la risposta Ã¨ 0 */
	andTable2(VALX, 1, VALX). /* se vale 1,la risposta Ã¨ il valore del congiunto di sx */
	andTable2(VALX, VALX, VALX).
	andTable2(VALX, VALY, (VALX & VALY)). /* altrimenti il valore Ã¨ l'and del valore dei 2 congiunti */


	orTable(1, _, 1). /*se il congiunto di sx vale 1 allora la risposta vale 1 */
	orTable(0, Y, RISP) :-valTerm(Y, RISP). /* se il congiunto di sx vale 1 allora la risposta vale il valore del congiunto di dx*/
	orTable(VALX, Y, RISP) :-valTerm(Y, VALY), !, /*se il congiunto di sx Ã¨ un termine qualsiasi allora si valuta il congiunto di dx */
		orTable2(VALX, VALY, RISP). /* e si guarda la tabella dell'and lungo la  colonna del secondo congiunto*/
	orTable2(_, 1, 1). /* se il congiunto di dx vale 1,la risposta Ã¨ 1 */
	orTable2(VALX, 0, VALX). /* se vale 0,la risposta Ã¨ il valore del congiunto di sx */
	orTable2(VALX, VALX, VALX).
	orTable2(VALX, VALY, (VALX | VALY)). /* altrimenti il valore Ã¨ l'and del valore dei 2 congiunti */

	/*Semplificazioni per l'implica*/

	/*se l'antecedente vale 0 allora l'implicazione e' una tautologia,quindi la risposta vale 1 */
	imTable(0, _, 1).

	/* se l'antecedente vale 1 allora la risposta vale il valore del conseguente*/
	imTable(1, Y, RISP) :- valTerm(Y, RISP), !.

	/*se l'antecedente Ã¨ un termine qualsiasi,allora si valuta il conseguente */
	imTable(VALX, Y, RISP) :- valTerm(Y, VALY), !,
		/*si decide in base al valore del conseguente*/ imTable2(VALX, VALY, RISP).

	/* se il conseguente vale: */
	imTable2(_, 1, 1). /* 1,la risposta Ã¨ 1 */
	imTable2(VALX, 0, ~ VALX). /* 0,siamo nel caso VALX -> false,cioe' ~ VALX*/
	imTable2(VALX, VALX, 1). /* come l'antecedente,siamo nel caso VALX->VALX,una tautologia */
	imTable2(VALX, VALY, (VALX => VALY)). /* una wff,allora il valore Ã¨ VALX->VALY */


	/**********************************************************

	valutazione di una swff

	**********************************************************/


	valSWFF(swff(S, X), swff(S, Y)) :-valTerm(X, Y).



	/***********************************************************

	  SOSTITUZIONE DI MASSACCI DI UNA FORMULA SEGNATA DENTRO UN'ALTRA FORMULA SEGNATA

	***********************************************************/



	/*
		data T X e S Y,esegue S Y[X/1],dove S Ã¨ il segno di Y
		data Fc X e S Y,esegue S Y[X/0],dove ...
	*/

	massacci(swff(t, X), swff(S, Y), swff(S, RISP)) :-massacciTrue(X, Y, RISP).
	massacci(swff(fc, X), swff(S, Y), swff(S, RISP)) :-massacciFalsoCerto(X, Y, RISP).


	/*
		data F X e S Y,esegue S Y{X/0},
		dove { } indica un rimpiazzamento che si ferma se si trova un connettivo implica o non
		e S e' il segno T o F MA NON Fc perche' Fc e' sinomimo di T non
	*/

	massacci(swff(f, X), swff(t, Y), swff(t, RISP)) :-massacciFalso(X, Y, RISP).
	massacci(swff(f, X), swff(f, Y), swff(f, RISP)) :-massacciFalso(X, Y, RISP).
	massacci(swff(f, _), swff(fc, Y), swff(fc, Y)). /* nelle formule Fc non vi Ã¨ rimpiazzamento */

	massacciTrue(X, X, 1).
	massacciTrue(_, 1, 1).
	massacciTrue(_, 0, 0).
	massacciTrue(_, Y, Y) :-atom(Y).
	massacciTrue(X, (SX & DX), (Y & Z)) :- massacciTrue(X, SX, Y),
		massacciTrue(X, DX, Z).

	massacciTrue(X, (SX <=> DX), (Y <=> Z)) :- massacciTrue(X, SX, Y),
		massacciTrue(X, DX, Z).

	massacciTrue(X, (SX | DX), (Y | Z)) :- massacciTrue(X, SX, Y),
		massacciTrue(X, DX, Z).
	massacciTrue(X, (SX => DX),(Y => Z)) :- massacciTrue(X, SX, Y),
		massacciTrue(X, DX, Z).
	massacciTrue(X, ~ SX, ~ Y) :- massacciTrue(X, SX, Y).

	/* data F X e S Y,esegue S Y[X/0] */
	massacciFalsoCerto(X, X, 0).
	massacciFalsoCerto(_, 0, 0).
	massacciFalsoCerto(_, 1, 1).
	massacciFalsoCerto(_, Y, Y) :- atom(Y).
	massacciFalsoCerto(X, (SX & DX), (Y & Z)) :- massacciFalsoCerto(X, SX, Y), !,
		massacciFalsoCerto(X, DX, Z), !.

	massacciFalsoCerto(X, (SX <=> DX), (Y <=> Z)) :- massacciFalsoCerto(X, SX, Y), !,
		massacciFalsoCerto(X, DX, Z), !.

	massacciFalsoCerto(X, (SX | DX), (Y | Z)) :- massacciFalsoCerto(X, SX, Y), !,
		massacciFalsoCerto(X, DX, Z), !.
	massacciFalsoCerto(X, (SX => DX),(Y => Z)) :- massacciFalsoCerto(X, SX, Y), !,
		massacciFalsoCerto(X, DX, Z), !.
	massacciFalsoCerto(X, ~ SX, ~ Y) :- massacciFalsoCerto(X, SX, Y), !.


	/* data F X e S Y,esegue S Y{X/0} */
	massacciFalso(X, X, 0).
	massacciFalso(_, 0, 0).
	massacciFalso(_, 1, 1).
	massacciFalso(_, Y, Y) :-atom(Y).
	massacciFalso(X, (SX & DX), (Y & Z)) :- massacciFalso(X, SX, Y), !,
		massacciFalso(X, DX, Z), !.
	massacciFalso(X, (SX | DX), (Y | Z)) :- massacciFalso(X, SX, Y), !,
		massacciFalso(X, DX, Z), !.
	massacciFalso(_, (SX => DX),(SX => DX)). /* su implica e non il rimpiazzamento si ferma */
	massacciFalso(_, (SX <=> DX), (SX <=> DX)). /* su implica equiv e non il rimpiazzamento si ferma */
	massacciFalso(_, ~ SX, ~ SX).



	/*
	 *	simplification(ListaCandidati,InsiemeDiSwff,InsiemeRisultato):
	 *	ListaCandidati e' l'insieme delle formule per cui si deve tentare il rimpiazzamento in
	 *	InsiemediSwff. Il risultato del rimpiazzamneto produce InsiemeRisultato.
	 *	Tipicamente la ListaCandidati e' la lista delle nuove formule generate in InsiemeDi Swff per
	 *	applicazione di una regola
	*/

	simplification([], X, X).
	simplification(_, X, X) :- \+ coerente(X),
		!.
	simplification([H|CANDIDATES], SWFFSET, NEWSET) :-
		substitute(H, SWFFSET, NEWSWFFSETFORH, NEWCANDIDATESFORH),
		append(CANDIDATES, NEWCANDIDATESFORH, NEWCANDIDATES),
		simplification(NEWCANDIDATES, NEWSWFFSETFORH, TEMPNEWSET),
		riprovaSimplification(H, NEWSWFFSETFORH, TEMPNEWSET, NEWSET),
		!.



	riprovaSimplification(_, NEWSWFFSETFORH, NEWSWFFSETFORH, NEWSWFFSETFORH). /* inutile tentare */
	riprovaSimplification(_, _, TEMPNEWSET, TEMPNEWSET) :- \+ coerente(TEMPNEWSET).
	riprovaSimplification(H, _, TEMPNEWSET, NEWSET) :- simplification([H], TEMPNEWSET, NEWSET), !.





	/*
	 *	substitute(F,SETOFSWFF,NEWSETOFSWFF,SETOFCANDIDATES)
	 *	F formula da rimpiazzare;
	 *	SETOFSWFF insieme in cui eseguire il rimpiazzamento;
	 *	NEWSETOFSWFF insieme di formule risultante;
	 *	SETOFCANDIDATES nuovi candidati da usare per eseguire un nuovo rimpiazzamento.
	*/

	substitute(swff(_, 0), SETOFSWFF, SETOFSWFF, []) :- !.
	substitute(swff(_, 1), SETOFSWFF, SETOFSWFF, []) :- !.
	substitute(F, SETOFSWFF, SETOFSWFF, []) :-
	/*
	 *	se F non appartiene all'insieme non si fanno sostituzioni
	 */
		\+ member(F, SETOFSWFF),
		!.

	substitute(F, SETOFSWFF, NEWSETOFSWFF, SETOFCANDIDATES) :-
	/*
	 *	se F appartiene all'insieme  si fanno sostituzioni
	 */
		goToSubstitute(F, SETOFSWFF, NEWSETOFSWFF, SETOFCANDIDATES).



	/*	goToSubstitute(F,[H|T],[RISP|NI],NEWCANDIDATES)
	rimpiazza F in [H|T].
	TheReplacementResult[RISP|NI] e' il risultato del rimpiazzamento.
	NEWCANDIDATES sono le nuove swff generate dal rimpiazzamento di F in [H|T]
	*/

	goToSubstitute(_, [], [], []).
	goToSubstitute(F, [F|T], [F|NI], NC) :- !,
		goToSubstitute(F, T, NI, NC).
	goToSubstitute(F, [H|T], TheReplacementResult, NEWCANDIDATES) :-
	/*
	 *	esegue il rimpiazzamento H[F/val],risultato in RR
	 */
		massacci(F, H, RR),
		!,
	/*
	 *	semplificazione di RR,risultato in RISP
	 */
		valSWFF(RR, RISP),
	/*
	 * TODO: se RISP e' formula contraddittoria
	 *	allora TheReplacementResult = [RISP | T] e
	 *		NEWCANDIDATES = []
	 *	altrimenti passo ricorsivo di rimpiazzamento di F in T.
	 *	possiamo cosÃ¬ evitare la chiamata a coerente fatta sopra.
	 */
		goToSubstitute(F, T, NI, NC),
		!,
		buidReplacementResult(RISP, NI, TheReplacementResult),
		!,
	/*
	 *	Se RISP <> da H,
	 *	allora RISP e' un nuovo candidato
	 */
		updatecandidates(NC, H, RISP, NEWCANDIDATES).
	/* forse qui va un ! */


	buidReplacementResult(swff(t, 1), NI, NI).
	buidReplacementResult(swff(f, 0), NI, NI).
	buidReplacementResult(swff(fc, 0), NI, NI).
	buidReplacementResult(RISP, NI, [RISP|NI]).

	/*
	 * Se H non e' stata modificata dal rimpiazzamento,allora non e' una formula candidata
	 */
	updatecandidates(NC, H, H, NC) :- !.

	/*
	 *	inutile mettere queste formule tra i candidati
	 */
	updatecandidates(NC, _, swff(t, 1), NC).
	updatecandidates(NC, _, swff(f, 0), NC).
	updatecandidates(NC, _, swff(fc, 0), NC).


	/*
	 *	inutile mettere queste formule tra i candidati,l'insieme Ã¨ inconsistente
	 */
	updatecandidates(_, _, swff(t, 0), []).
	updatecandidates(_, _, swff(f, 1), []).
	updatecandidates(_, _, swff(fc, 1), []).
	updatecandidates(NC, _, RIS, [RIS|NC]).


	/**************************************************************

		Predicati usati per implementare la regolarita'

	**************************************************************/


	/* valore della wfff X nella lista di swff M che funge da modello:
	   NOTA BENE: formule del tipo swff(t,1) etc NON VENGONO CONSIDERATE
	*/

	realizzata(swff(t, WFF), M) :- atom(WFF), !, memberchk(swff(t, WFF), M).

	/*una F-atomica Ã¨ realizzata se la corrispondente T non appartiene al modello*/
	realizzata(swff(f, WFF), M) :- atom(WFF), !, \+ member(swff(t, WFF), M).

	realizzata(swff(t, (SX & DX)), M) :- realizzata(swff(t, SX), M), !, realizzata(swff(t, DX), M), !.
	realizzata(swff(t, (SX <=> DX)), M) :- realizzata(swff(t, (SX => DX)), M), !, realizzata(swff(t, (DX => SX)), M), !.
	realizzata(swff(t, (SX | _)), M) :- realizzata(swff(t, SX), M), !.
	realizzata(swff(t, (_ | DX)), M) :- realizzata(swff(t, DX), M), !.
	realizzata(swff(t, (_ => DX)), M) :- realizzata(swff(t, DX), M), !.
	realizzata(swff(t, (SX => _)), M) :- realizzata(swff(f, SX), M), !.
	realizzata(swff(t, ~ WFF), M) :- realizzata(swff(f, WFF), M), !.

	realizzata(swff(f, (SX | DX)), M) :- realizzata(swff(f, SX), M), !, realizzata(swff(f, DX), M), !.
	realizzata(swff(f, (SX & _)), M) :- realizzata(swff(f, SX), M), !.
	realizzata(swff(f, (_ & DX)), M) :- realizzata(swff(f, DX), M), !.

	realizzata(swff(f, (SX <=> DX)), M) :- realizzata(swff(f, ((SX => DX) & (DX => SX))), M), !.

	realizzata(swff(f, (SX => DX)), M) :- realizzata(swff(t, SX), M), !, realizzata(swff(f, DX), M), !.
	realizzata(swff(f, ~ WFF), M) :- realizzata(swff(t, WFF), M), !.


	/*********************************************************************************

			PREDICATI PER IMPLEMENTARE L'INTUIZIONISMO

	*********************************************************************************/

	/*
			reapply(RESULT,MODEL) VERIFICA L'INSIEME RESULT PRIMA DI TENTARE DI COSTRUIRE IL CONTROMODELLO

	se RESULT E' coerente lo suddivide in 2
	insiemi. BACK e RESTO.
	reapply fallisce se RESULT rappresenta una formula valida
	*/

	reapply(SET, MODEL, SPAZIO, IdxNewAtom) :- coerente(SET), !,
		buildBack(SET, BACK, RESTO), !,
		intControModello(BACK, RESTO, ModelOfTheRule, SPAZIO+1, WhichBranch, IdxNewAtom), !,
		/*SIBLINGS=nosiblings significa che */ kripke(SET, BACK, WhichBranch, ModelOfTheRule, MODEL), !.
	/*una delle formule di tipo 4 Ã¨ stata*/
	/*espansa con il ramo safe.*/
	/*Questo ha rilevanza per come si*/
	/*costruisce il contromodello*/

	/* reapply(_,_,SPAZIO,_):-	tab(3*SPAZIO-3),writeln('CLOSED SET'),fail.	*/

	/*
			COSTRUISCE CONTROMODELLO INTUIZIONISTA

	intControModello(BACK,RESTO,COUNTERMODEL,SPAZIO,jumpbranch | safebranch,IdxNewAtom):
	costruisce il contromodello a BACK unione RESTO.
	Se il contromodello non esiste
	allora COUNTERMODEL contiene [no countermodel].
	intControModello viene chiamato su un insieme coerente.
	SPAZIO: variabile usata per la stampa;
	jumpbranch | safebranch : specifica se la conclusione di rule ha Sc o no. L'informazione e' usata da
	continuaIterazione per trattare T->-> e T->non;
	IdxNewAtom e' un intero che serve a generare sistematicamente nuovi atomi per T->->.
	*/

	intControModello([], RESTO, Contromodello, _SPAZIO, safebranch, _) :-
	/*
		tab(3*SPAZIO-3),
		writeln('Open set,searching for a backtraking point...\n\n'),
		*/
		filtraNonAtomiche(RESTO, Contromodello).
	/* se non ci sono regole da applicare
		allora dato che RESTO Ã¨ coerente,il contromodello Ã¨ RESTO
		e la conclusione Ã¨ considerata safe
	*/

	intControModello([swff(fperm, list(Wff, Atoms))], RESTO, MODEL, SPAZIO, WhichBranch, IdxNewAtom) :- !,
		/* nota che adesso rule Ã¨ a 5 parametri */ rule(swff(fperm, list(Wff, Atoms)), RESTO, RESULT, WhichBranch, IdxNewAtom),

		write('Main SWFF: '),
		printSWFF(swff(f, Wff)),
		write(' with constant atoms: '),
		write(Atoms),
		writeln('\n'),
		printSWFFSet(RESULT, SPAZIO),
		writeln('\n'),

		NextIdx is IdxNewAtom + 1,
		reapply(RESULT, RightSuccessor, SPAZIO, NextIdx),
		!,
		filtraNonAtomiche(RightSuccessor, AtomicRightSuccessor),
		rimuoviAtomiOpposti(Atoms, AtomicRightSuccessor, RightSuccessorFiltered),
		!,
		union(Atoms, RightSuccessorFiltered, LeftSuccessor),
		/*adesso il fratello e' costruito correttamente*/

		/*filtra le non Atomiche dall'insieme nodo che e' padre*/
		filtraNonAtomiche([swff(f, Wff)|RESTO], RESTOFiltrato),

		/*incolla i due modelli in uno*/
		union(RESTOFiltrato, [LeftSuccessor, AtomicRightSuccessor], MODEL).



	intControModello([H], RESTO, MODEL, SPAZIO, WhichBranch, IdxNewAtom) :-
		!,
		/* nota che adesso rule Ã¨ a 5 parametri */ rule(H, RESTO, RESULT, WhichBranch, IdxNewAtom),

		write('Main SWFF: '),
		printSWFF(H),
		writeln('\n'),
		printSWFFSet(RESULT, SPAZIO),
		writeln(' '),

		NextIdx is IdxNewAtom + 1,
		reapply(RESULT, MODEL, SPAZIO, NextIdx),
		!.

	intControModello([H, T|BACK], RESTO, MODEL, SPAZIO, WhichBranch, IdxNewAtom) :-
		append([T|BACK], RESTO, SET),
		intControModello([H], SET, MODELH, SPAZIO, HWhichBranch, IdxNewAtom), !,
		/* ci domandiamo se davvero*/ continuaIterazione([T|BACK], [H|RESTO], MODELH, MODEL, SPAZIO, HWhichBranch, IdxNewAtom, WhichBranch), !.
	/* abbiamo bisogno di fare*/
	/* backtracking su [T|BACK]*/


	/*
	se H e' di tipo 4 ed il contromodello MODELH e' stato ottenuto applicando il ramo safe,
	allora non c'e' bisogno di alcuna iterazione ed il contromodello restituito Ã¨ []
	*/

	continuaIterazione(_, [H|_], MODELH, MODELH, _, safebranch, _, safebranch) :- tipo4(H).

	/*
	se l'intero insieme di formule sia realizzato da MODELH,
	allora questo Ã¨ il modello dell'intero insieme e non c'e' bisogno di andare avanti
	con l'iterazione. L'applicazione della regola puo' essere considerata safe
	*/

	/*	nota che questa condizione se attivata riguarda H di tipo 3 o 4.
		Nel caso di tipo 4 riguarda l'applicazione non safe della regola
	*/

	continuaIterazione([T|BACK], [_|RESTO], MODELH, NEWMODELH, _, jumpbranch, _, safebranch) :-

		/*ricostruisce l'insieme nodo,non considera H*/ append([T|BACK], RESTO, Set),
		/*estrae le formule segnate F	*/ falseSWFF(Set, FSWFF),
		/*se tutte le F-swff di SET sono classiche e*/ tutteClassiche(FSWFF),
		/*realizzate fermiamo il backtrack*/ insiemeRealizzato(FSWFF, MODELH),
		conservaTatomiche(RESTO, FilteredSet),
		append(FilteredSet, MODELH, NEWMODELH).
	/*altrimenti lo proseguiamo.
	  Nota che quando fermiamo il backtrack
	  Ã¨ come se avessimo applicato una regola safe
	*/



	/*
	se  il contromodello MODELH e' stato ottenuto applicando il ramo Sc,
	allora andiamo avanti con il backtrack
	*/
	continuaIterazione([T|BACK], [H|RESTO], MODELH, MODEL, SPAZIO, jumpbranch, IdxNewAtom, WhichBranch) :-
		writeln('Found a backtacking point:\n'),
		union([H, T|BACK], RESTO, NodeSet),
		printSWFFSet(NodeSet, SPAZIO-1),
		writeln(' '),
		intControModello([T|BACK], [H|RESTO], MODELRESTO, SPAZIO, WhichBranch, IdxNewAtom), !,
		gluesiblings(WhichBranch, BACK, MODELH, MODELRESTO, MODEL), !.

	gluesiblings(jumpbranch, [], MODELH, MODELRESTO, [MODELH, MODELRESTO]).
	gluesiblings(jumpbranch, _, MODELH, MODELRESTO, [MODELH| MODELRESTO]).
	gluesiblings(safebranch, _, _, MODELRESTO, MODELRESTO).
	/*
		falseSWFF(Set,FSWFF),estrae le formule segnate F
	*/
	falseSWFF([], []).
	falseSWFF([swff(f, X)|T], [swff(f, X)|R]) :- !, falseSWFF(T, R), !.
	falseSWFF([_|T], R) :- !, falseSWFF(T, R), !.

	/*
	tutteClassiche(FSWFF),stabilisce se tutte le F-swff di SET sono classiche
	*/
	tutteClassiche([]).
	tutteClassiche([H|T]) :- isSwffClassic(H, 1), tutteClassiche(T), !.

	/*
	insiemeRealizzato(FSWFF,MODELH),stabilisce se MODELH realizza le formule classiche di FSWFF
	*/
	insiemeRealizzato([], _).
	insiemeRealizzato([H|T], MODELH) :- realizzata(H, MODELH), insiemeRealizzato(T, MODELH), !.


	/************************************************************************

			COSTRUZIONE DI UNA PARTE DEL CONTROMODELLO

	kripke(SET,BACK,WhichBranch,ModelOfTheRule,MODEL)
	SET Ã¨ l'insieme di formule. Ha un ruola solo quando Ã¨ un saturato,
	BACK 	nei casi in cui WhichBranch= jumpbranch permette di sapere se TheModelOfRESULT
		 	Ã¨ una lista che rappresenta esattamente uno stato oppure una lista in cui ogni componente Ã¨ una lista
		 	che rappresenta uno stato
	WhichBranch 	vale safebranch se RESULT Ã¨ ottenuto da un ramo safe,
			vale jumpbranch se RESULT Ã¨ ottenuto da un ramo di jump
	TheModelOfRESULT il modello relativo a RESULT
	MODEL il modello restituito,
	************************************************************************/

	kripke(_, _, safebranch, MODEL, MODEL).
	kripke(SET, [_], jumpbranch, TheModelOfRESULT, MODEL) :- filtraNonAtomiche(SET, FilteredSet),
		append(FilteredSet, [TheModelOfRESULT], MODEL).
	kripke(SET, [_, _|_], jumpbranch, TheModelOfRESULT, MODEL) :- filtraNonAtomiche(SET, FilteredSet),
		append(FilteredSet, TheModelOfRESULT, MODEL).
	kripke(_, _, _, _, _) :- writeln('CHIAMATA A KRIPKE NON CORRISPONDE'),
		abort.

	/*
	STAMPA IL MODELLO DI KRIPKE
	*/

	printKripke([], _).
	printKripke([swff(S, WFF)|T], SPAZIO) :- printSWFFSet([swff(S, WFF)], SPAZIO),
		!,
		printKripke(T, SPAZIO),
		!.
	printKripke([[X|Y]|Z], SPAZIO) :- writeln('-- end world -- '),
		printKripke([X|Y], SPAZIO+1),
		!,
		printKripke(Z, SPAZIO),
		!.

	printKripke([[]|Z], SPAZIO) :- writeln(' '),
		printKripke([swff(t, emptyNode)], SPAZIO+1),
		!,
		printKripke(Z, SPAZIO),
		!.

	printKripke(_, _) :- writeln('printKripke NON CORRISPONDE'), abort.



	/*
	coerente e' vero se la lista e' vuota oppure oppure ha solo un elemento oppure
	Y e' coerente e Y non contiene una formula di segno opposto a X
	*/

	coerente([]).

	coerente(L) :- \+ member(swff(t, 0), L),
		\+ member(swff(f, 1), L),
		\+ member(swff(fc, 1), L).



	/*

	buildBack(SET,BACK,RESTO): costruisce l'insieme di backtrack. Se SET contiene almeno una swff di tipo 1 o 2 allora BACK
	ha solo 1 formula. Se non contiene formule di tipo 1 o 2 allora vengono collezionate in BACK tutte quelle di tipo 3 e 4.
	Se questi tipi di swff non sono presenti in SET,allora si cerca una formula di tipo 5 o 6. Se neppure formule di
	questo tipo esistono allora  SET non Ã¨ espandibile.

	*/

	buildBack(SET, [B|ACK], RESTO) :- cercaTipiAlphaBeta(SET, [B|ACK], RESTO).

	/*
	se cercaPermanenceFormula(SET,[B|ACK],RESTO) ha successo,
	allora B contiene una F-> formula a cui applicare una delle permanence rules
	*/
	buildBack(SET, [swff(fperm, list(X, TheConstantAtoms))], RESTO) :-
		cercaPermanenceFormula(SET, [swff(f, X)], RESTO, TheConstantAtoms).


	buildBack(SET, [B|ACK], RESTO) :- cercaBack(SET, [B|ACK], RESTO).
	buildBack(SET, [B|ACK], RESTO) :- cercaTipi5e6(SET, [B|ACK], RESTO).
	buildBack(SET, [], SET). /* attenzione: questo caso unfica anche con tutti gli altri precedenti,non Ã¨ in esclusione */

	/*
		glueModels: incolla i contromodelli
	*/

	glueModels(RADICE, SX, DX, [RADICE, SX, DX]).


	/*

	STABILISCE SE UNA SWFF E' CLASSICA

	*/

	isSwffClassic(swff(f, WFF), RISP) :- isWffClassic(WFF, RISP), !.
	isSwffClassic(swff(t, WFF), RISP) :- isWffClassic(WFF, RISP), !.
	isSwffClassic(swff(fc, _), 0). /* le Fc non sono classiche */


	isWffClassic(1, 1) :- writeln('isWFFClassic chiamato con wff uguale a 1').
	isWffClassic(0, 1) :- writeln('isWFFClassic chiamato con wff uguale a 0').
	/* le formule 1 e 0 sono classiche,questo caso non dovrebbe mai verificarsi */
	isWffClassic(WFF, 1) :-atom(WFF), !.
	isWffClassic((LEFT & RIGHT), 1) :- isWffClassic(LEFT, 1), !, isWffClassic(RIGHT, 1), !.
	isWffClassic((LEFT | RIGHT), 1) :- isWffClassic(LEFT, 1), !, isWffClassic(RIGHT, 1), !.
	isWffClassic(WFF, 0) :- \+ isWffClassic(WFF, 1), !. /* in tutti gli altri casi non Ã¨ wff classica */

	/*
	cercaTipiAlphaBeta(X,BACK,RESTO):
	cerca una formula di tipo 1 o 2 da espandere. Restituisce in BACK una formula e in RESTO le restanti formule di X
	cioÃ¨ X=BACK unione RESTO
	*/

	cercaTipiAlphaBeta(X, BACK, RESTO) :- cercaTipiAlphaBetaModel(X, X, BACK, RESTO).

	/*
	cercaTipiAlphaBetaModel usa il primo argomento come modello,quindi non viene mai modificato
	*/

	/*passi base*/
	cercaTipiAlphaBetaModel(_, [], [], []).
	cercaTipiAlphaBetaModel(MODEL, [swff(S, 1)|T], BACK, RESTO) :- cercaTipiAlphaBetaModel(MODEL, T, BACK, RESTOdiT),
		append(RESTOdiT, [swff(S, 1)], RESTO).
	cercaTipiAlphaBetaModel(MODEL, [swff(S, 0)|T], BACK, RESTO) :- cercaTipiAlphaBetaModel(MODEL, T, BACK, RESTOdiT),
		append(RESTOdiT, [swff(S, 0)], RESTO).
	cercaTipiAlphaBetaModel(MODEL, [swff(S, A)|T], BACK, RESTO) :- atom(A), !,
		cercaTipiAlphaBetaModel(MODEL, T, BACK, RESTOdiT),
		append(RESTOdiT, [swff(S, A)], RESTO).
	/*Arrivati qui sappiamo che A non Ã¨ atomica */

	/* CASO T->atom: si potrebbe trattare come caso speciale e  tirar via dall'insieme di formule di tipo alpha,
	cosi' da non farlo corrispondere anche al caso seguente:
	L'insieme BACK conterra' una T (p->A) se T p Ã¨ in X
	*/

	cercaTipiAlphaBetaModel(MODEL, [swff(t, (X => Y))|B], [swff(t, (X => Y))], B) :- atom(X),
		memberchk(swff(t, X), MODEL).

	cercaTipiAlphaBetaModel(_, [A|B], [A], B) :- tipoalpha(A).

	/*arrivati qua sappimo che A Ã¨ di tipo beta*/

	cercaTipiAlphaBetaModel(MODEL, [A|B], BACK, RESTO) :- tipobeta(A),
		/* A classica ma non realizzata,*/ isSwffClassic(A, 1),
		/* A e' swff tipo beta da espandere*/ \+ realizzata(A, MODEL), !,
		/* vediamo se troviamo una swff alpha*/ findWFFalpha(B, BACKdiB, RESTOdiB),
		/* predicato di supporto*/ sistema(A, B, BACKdiB, RESTOdiB, BACK, RESTO).


	cercaTipiAlphaBetaModel(_, [A|B], BACK, RESTO) :- tipobeta(A),
		/* Arrivati qui sappiamo che A e' di tipo beta */ isSwffClassic(A, 0), !,
		/* Arrivati qui sappiamo che A non e' classica*/
		/* quindi e' da espandere*/
		/* vediamo se troviamo una swff alpha*/ findWFFalpha(B, BACKdiB, RESTOdiB),
		/* predicato di supporto*/ sistema(A, B, BACKdiB, RESTOdiB, BACK, RESTO).

	/*arrivati qua sappiamo che A Ã¨ di tipo beta ed Ã¨ realizzata dal modello sottostante,
	quindi non c'e' bisogno di espanderla,cerchiamo un'altra formula nell'insieme B*/

	cercaTipiAlphaBetaModel(MODEL, [A|B], BACK, RESTO) :- cercaTipiAlphaBetaModel(MODEL, B, BACK, RESTOdiB),
		append(RESTOdiB, [A], RESTO).

	/*in B non c'e' wff di tipo alpha. Allora l'insieme di backtrack Ã¨ fatto dalla wff A,che Ã¨ di tipo beta,
	la parte restante Ã¨ nell'insieme B
	*/

	sistema(A, B, [], _, [A], B).

	/*se una wff di tipo alpha Ã¨ stata trovata in B,allora l'insieme di backtracking Ã¨ fatto dalla formula
	alpha appena trovata,RESTO Ã¨ A+RESTOdiB*/

	sistema(A, _, BACKdiB, RESTOdiB, BACKdiB, [A| RESTOdiB]).



	/*
	cercaBack(X,BACK,RESTO) mette in BACK le formule di X che sono di tipo 3 e 4,le restanti le mette in RESTO.
	Si comporta come segue:  colleziona in T3 le formule di tipo3 di X,colleziona in T4 le formule di tipo 4 di X,
	costruisce l'insieme di backtracking secondo la seguente strategia: se T3 ha una F-formula che Ã¨ l'unica F-formula di X,
	allora non c'e' biogno di fare backtrack e quindi BACK=T3; se X non contiene F-formule
	allora BACK conterra esattamente una formula di T4. Se i precedenti casi non valgono allora
	BACK=T3 unione T4.
	*/

	cercaBack(X, BACK, RESTO) :- prendeTipo3(X, T3, R3),
		prendeTipo4(R3, T4, R4),
		costruisciBack(T3, T4, R4, BACK, RESTO).


	prendeTipo3([], [], []). /* l'insieme di ricerca Ã¨ vuoto */
	prendeTipo3([A|T], [A|T3], R3) :- tipo3(A),
		!,
		prendeTipo3(T, T3, R3).
	/*se arriviamo qui A non Ã¨ di tipo 3,quindi le swff di tipo 3 si trovano in T e A e' una delle formule di RESTO */

	prendeTipo3([A|T], T3, [A|R3]) :- prendeTipo3(T, T3, R3).

	prendeTipo4([], [], []). /* l'insieme di ricerca Ã¨ vuoto */
	prendeTipo4([A|T], [A|T4], R4) :- tipo4(A),
		!,
		prendeTipo4(T, T4, R4).
	/*se arriviamo qui A non Ã¨ di tipo 4,quindi le swff di tipo 4 si trovano in T e A e' una delle formule di RESTO */

	prendeTipo4([A|T], T4, [A|R4]) :- prendeTipo4(T, T4, R4).


	costruisciBack([A], T4, R4, [A], RESTO) :- haZeroFFormule(R4), /* L'insieme X ha esattamente una swff di tipo 3 */
		!,
		append(T4, R4, RESTO).

	costruisciBack([], [A|T4], R4, [A], RESTO) :- haZeroFFormule(R4), /* X ha zero F-swff,in BACK metto una swff di tipo 4 */
		!,
		append(T4, R4, RESTO).

	/* qui si deve gestire anche il caso che X non abbia swff di tipo 3 e 4 */

	costruisciBack(T3, T4, R4, BACK, R4) :- append(T3, T4, BACK).




	/*
	cercaTipi5e6(X,BACK,RESTO) cerca una formula di tipo 5 o 6 da espandere.
	Restituisce in BACK una formula e in RESTO le restanti formule di X
	cioÃ¨ X=BACK unione RESTO
	*/

	/*passi base*/
	cercaTipi5e6([], [], []).
	cercaTipi5e6([swff(S, 1)|T], BACK, RESTO) :- cercaTipi5e6(T, BACK, RESTOdiT),
		append(RESTOdiT, [swff(S, 1)], RESTO).

	cercaTipi5e6([swff(S, 0)|T], BACK, RESTO) :- cercaTipi5e6(T, BACK, RESTOdiT),
		append(RESTOdiT, [swff(S, 0)], RESTO).

	cercaTipi5e6([swff(S, A)|T], BACK, RESTO) :- atom(A), !,
		cercaTipi5e6(T, BACK, RESTOdiT),
		append(RESTOdiT, [swff(S, A)], RESTO).
	/*Arrivati qui sappiamo che A non Ã¨ atomica,
	  verifichiamo non sia una T(p->B) cioe' una swff che si comporta come un'atomica perche' nell'insieme
	  manca T p
	*/

	cercaTipi5e6([swff(t, (A => Y))|T], BACK, RESTO) :- atom(A), !,
		cercaTipi5e6(T, BACK, RESTOdiT),
		append(RESTOdiT, [swff(t, (A => Y))], RESTO).


	cercaTipi5e6([A|B], [A], B) :- tipo5(A).

	/*arrivati qua sappimo che A non e' di tipo 5*/

	cercaTipi5e6([A|B], BACK, RESTO) :- tipo6(A),
		/*trovata una swff tipo 6 da espandere*/
		/*vediamo se troviamo una swff tipo 5*/ findWFFtipo5(B, BACKdiB, RESTOdiB),
		/*predicato di supporto*/ sistema(A, B, BACKdiB, RESTOdiB, BACK, RESTO).


	/* arrivati qui sappiamo che A non e' neppure di tipo 6,quindi sara' del tipo swff(t,im(p,H)),
	cioÃ¨ una formula che si comporta come un'atomica */

	cercaTipi5e6([A|B], BACK, [A|RESTOdiB]) :- cercaTipi5e6(B, BACK, RESTOdiB).



	findWFFtipo5([], [], []).
	findWFFtipo5([H|T], [H], T) :- tipo5(H).
	findWFFtipo5([H|T], X, [H|Y]) :- findWFFtipo5(T, X, Y).




	/*
		REGOLE INTUIZIONISTE

	rule(H,S,RESULT,safebranch | jumpbranch):
	H e' la main swff a cui applicare la regola;
	S e' l'insieme;
	NEWS e' l'insieme rislutato dopo la semplificazione
	safebranch se la conclusione non ha Sc,altrimenti jumpbranch
	*/


	/*regola T and*/
	rule(swff(t, (X & Y)), S, NEWS, safebranch, _) :-
		!,
		estraiListaTAnd((X & Y), ListaAnd),
		union(ListaAnd, S, S1),
		semplificazioneBreve(ListaAnd, S1, NEWS).

	rule(swff(t, (X <=> Y)), S, NEWS, safebranch, _) :- !,
		semplificazioneCerta([swff(t, (X => Y)), swff(t, (Y => X))],
			[swff(t, (X => Y)), swff(t, (Y => X))|S], NEWS).

	/*regola T or */
	rule(swff(t, (X | Y)), S, NEWS, safebranch, _) :- !,
		estraiListaTOr((X | Y), ListaOr),
		trattaListaTor(ListaOr, S, NEWS).










	/*regola T im Atom */
	rule(swff(t, (A => Y)), S, NEWS, safebranch, _) :- atom(A),
		!,
		semplificazioneCerta([swff(t, Y)], [swff(t, Y)|S], NEWS).

	/*regola T im and*/
	rule(swff(t, ((A & B) => Y)), S, NEWS, safebranch, _) :-
		!,
		semplificazioneBreve([swff(t, (A => (B => Y)))], [swff(t, (A => (B => Y)))|S], NEWS).

	/*regola T im equiv*/
	rule(swff(t, ((A <=> B) => Y)), S, NEWS, safebranch, _) :-
		!,
		semplificazioneCerta([swff(t, (((A => B) & (B => A)) => Y))],
			[swff(t, (((A => B) & (B => A)) => Y))|S], NEWS).

	/*regola T im or*/
	rule(swff(t, ((A | B) => Y)), S, NEWS, safebranch, _) :-
		!,
		semplificazioneBreve([swff(t, (A => Y)), swff(t, (B => Y))],
			[swff(t, (A => Y)), swff(t, (B => Y))|S], NEWS).

	/*regola T im im*/
	rule(swff(t, ((_ => _) => C)), S, NEWS, safebranch, _) :- semplificazione([swff(t, C)], [swff(t, C)|S], NEWS),
		write('Right branch of T->->,').

	/*regola T im im*/
	rule(swff(t, ((A => B) => C)), S, NEWS, jumpbranch, _) :-
		atom(B),
		!,
		messageOnClosedSet([swff(t, ((A => B) => C))|S]),
		write('Left branch of T->->,'),
		partecerta(S, SCERTO),
		!,
		semplificazione([swff(t, A), swff(f, B), swff(t, (B => C))],
			[swff(t, A), swff(f, B), swff(t, (B => C))|SCERTO], NEWS).

	/*regola T im im*/
	rule(swff(t, ((A => B) => C)), S, NEWS, jumpbranch, IdxNewAtom) :-
		!,
		messageOnClosedSet([swff(t, ((A => B) => C))|S]),
		write('Left branch of T->->,'),
		partecerta(S, SCERTO),
		!,
		atomic_concat(newAt, IdxNewAtom, NewAtom),
		semplificazione([swff(t, A)],
			[swff(t, A), swff(f, NewAtom), swff(t, (B => NewAtom)), swff(t, (NewAtom => C))|SCERTO], NEWS).

	/*regola T im non*/
	rule(swff(t, (~ _ => C)), S, NEWS, safebranch, _) :- semplificazione([swff(t, C)], [swff(t, C)|S], NEWS),
		write('Right branch of T->not,').


	/*regola T im non*/
	rule(swff(t, (~ A => B)), S, NEWS, jumpbranch, _) :- !,
		messageOnClosedSet([swff(t, (~ A => B))|S]),
		write('Left branch of T->not,'),
		partecerta(S, SCERTO), !,
		semplificazione([swff(t, A)], [swff(t, A)|SCERTO], NEWS).

	/*regola T non */
	rule(swff(t, ~X), S, NEWS, safebranch, _) :- !,
		semplificazioneBreve([swff(fc, X)], [swff(fc, X)|S], NEWS).

	/* F or*/
	rule(swff(f, (X | Y)), S, NEWS, safebranch, _) :-
		!,
		estraiListaFOr((X | Y), ListaOr),
		union(ListaOr, S, S1),
		semplificazioneBreve(ListaOr, S1, NEWS).



	rule(swff(f, (X & _)), S, NEWS, safebranch, _) :- write('Left branch of F and,'),
		semplificazioneBranch([swff(f, X)], [swff(f, X)|S], NEWS).

	rule(swff(f, (X & Y)), S, NEWS, safebranch, _) :- !,
		messageOnClosedSet([swff(f, (X & Y))|S]),
		write('Rigth branch of F and,'),
		semplificazioneBranch([swff(f, Y)], [swff(f, Y)|S], NEWS).



	rule(swff(f, (X <=> Y)), S, NEWS, safebranch, _) :- write('Left branch of F equiv,'),
		semplificazioneBranch([swff(f, (X => Y))], [swff(f, (X => Y))|S], NEWS).

	rule(swff(f, (X <=> Y)), S, NEWS, safebranch, _) :- !,
		messageOnClosedSet([swff(f, (X <=> Y))|S]),
		write('Right branch of F equiv,'),
		semplificazioneBranch([swff(f, (Y => X))], [swff(f, (Y => X))|S], NEWS).

	/* F im */
	rule(swff(f, (X => Y)), S, NEWS, jumpbranch, _) :- !,
		write('F->,'),
		partecerta(S, SCERTO),
		!,
		semplificazione([swff(t, X), swff(f, Y)], [swff(t, X), swff(f, Y)|SCERTO], NEWS).

	/* Fperm im: implementa le regole permanence per F-> */
	rule(swff(fperm, list((XL => XR), ConstantAtoms)), S, NEWS, safebranch, _) :-
		!,
		writeln(''),
		write('Permanence rule applied to the F-> swff: '),
		printSWFF(swff(f, (XL => XR))),
		mainConsequent(ConstantAtoms, swff(f, (XL => XR)), SetWithTheNewFimplica),
		union(SetWithTheNewFimplica, S, NEWS).

	/* F non */
	rule(swff(f, ~X), S, NEWS, jumpbranch, _) :- !,
		write('F non,'),
		partecerta(S, SCERTO),
		!,
		semplificazione([swff(t, X)], [swff(t, X)|SCERTO], NEWS).

	/* Fperm non: implementa le regole permanence per F-non */
	rule(swff(fperm, list(~X, ConstantAtoms)), S, NEWS, safebranch, _) :-
		!,
		write('Permanence rule applied to the F~ swff: '),
		printSWFF(swff(f, ~X)),
		mainConsequent(ConstantAtoms, swff(f, ~X), SetWithTheNewFimplica),
		union(SetWithTheNewFimplica, S, NEWS).

	/* Fc or*/
	rule(swff(fc, (X | Y)), S, NEWS, safebranch, _) :-
		!,
		semplificazioneBreve([swff(fc, X), swff(fc, Y)], [swff(fc, X), swff(fc, Y)|S], NEWS).


	/* Fc and */
	rule(swff(fc,(X & _)), S, NEWS, jumpbranch, _) :- partecerta(S, SCERTO),
		semplificazioneCerta([swff(fc, X)], [swff(fc, X)|SCERTO], NEWS).

	/* Fc and */
	rule(swff(fc, (X & Y)), S, NEWS, jumpbranch, _) :- !,
		messageOnClosedSet([swff(fc, (X & Y))|S]),
		partecerta(S, SCERTO),
		semplificazioneCerta([swff(fc, Y)], [swff(fc, Y)|SCERTO], NEWS).

	/* Fc equiv */
	rule(swff(fc, (X <=> Y)), S, NEWS, jumpbranch, _) :- partecerta(S, SCERTO),
		semplificazioneCerta([swff(fc, (X => Y))],
			[swff(fc, (X => Y))|SCERTO], NEWS).

	/* Fc equiv */
	rule(swff(fc, (X <=> Y)), S, NEWS, jumpbranch, _) :-
		!,
		messageOnClosedSet([swff(fc, (X <=> Y))|S]),
		partecerta(S, SCERTO),
		semplificazioneCerta([swff(fc, (Y => X))], [swff(fc, (Y => X))|SCERTO], NEWS).


	/* Fc im */
	rule(swff(fc, (X => Y)), S, NEWS, jumpbranch, _) :-
		!,
		partecerta(S, SCERTO),
		!,
		semplificazioneBreve([swff(t, X), swff(fc, Y)], [swff(t, X), swff(fc, Y)|SCERTO], NEWS).

	/* Fc non */
	rule(swff(fc, ~X), S, NEWS, jumpbranch, _) :- !,
		write('Fc non,'),
		partecerta(S, SCERTO),
		!,
		semplificazioneBreve([swff(t, X)], [swff(t, X)|SCERTO], NEWS).




	/*

		partecerta: produce Sc dato S. Non vengono copiati T e Fc atomiche

	*/



	partecerta([], []). /* passo base */

	partecerta([swff(t, X)|T], RESTO) :- atom(X),
		!,
		partecerta(T, RESTO).

	partecerta([swff(t, X)|T], [swff(t, X)|RESTO]) :-
		partecerta(T, RESTO),
		!.

	partecerta([swff(fc, X)|T], RESTO) :- atom(X),
		!,
		partecerta(T, RESTO),
		!.

	partecerta([swff(fc, X)|T], [swff(fc, X)|RESTO]) :- partecerta(T, RESTO), !.

	partecerta([swff(f, _)|T], RESTO) :- partecerta(T, RESTO), !.




	/*
	PREDICATI DI SUPPORTO
	*/

	haZeroFFormule([]). /* Un insieme vuoto ha zero F-swff */
	haZeroFFormule([swff(t, _)|L]) :- haZeroFFormule(L).
	haZeroFFormule([swff(fc, _)|L]) :- haZeroFFormule(L).



	appartenenza(X, L, 1) :- memberchk(X, L), !.
	appartenenza(X, L, 0) :- \+ member(X, L).


	wff(X) :- wffIsCorrect(X), printWFF(X).

	wffIsCorrect(X) :-atom(X).
	wffIsCorrect((X & Y)) :- wffIsCorrect(X), wffIsCorrect(Y).
	wffIsCorrect((X <=> Y)) :- wffIsCorrect(X), wffIsCorrect(Y).
	wffIsCorrect((X | Y)) :- wffIsCorrect(X), wffIsCorrect(Y).
	wffIsCorrect((X => Y)) :- wffIsCorrect(X), wffIsCorrect(Y).
	wffIsCorrect((~X)) :- wffIsCorrect(X).

	/****************************************
	*                                       *
	*    PREDICATI PER IL PRUNING           *
	*                                       *
	****************************************/


	/*
		Predicato specializzato usato per calcolare il contesto congiuntivo di una T-and
	*/

	ccTAnd(0, [swff(t, 0)]). /* aggiunto nella versione V13 */
	ccTAnd(1, [swff(t, 1)]). /* aggiunto nella versione V13 */
	ccTAnd(F, [swff(t, F)]) :- atom(F).
	ccTAnd((X & Y), SET) :- ccTAnd(X, XC),
		ccTAnd(Y, YC),
		append(XC, YC, SET).
	ccTAnd(WFF, [swff(t, WFF)]).

	/*
		Predicato specializzato usato per calcolare il contesto congiuntivo di una F-or
	*/
	ccFor(0, [swff(f, 0)]). /* aggiunto nella versione V13 */
	ccFor(1, [swff(f, 1)]). /* aggiunto nella versione V13 */
	ccFor(F, [swff(f, F)]) :- atom(F).
	ccFor((X | Y), SET) :- ccFor(X, XC),
		ccFor(Y, YC),
		append(XC, YC, SET).
	ccFor(WFF, [swff(f, WFF)]).


	pruningSetSwff([], []).
	pruningSetSwff([H|T], [HP|TP]) :- pruning(H, HP),
		pruningSetSwff(T, TP), !.


	/*

		CONTESTO CONGIUNTIVO DI UNA FORMULA

	cc(F,I),dove:

	F e' una formula segnata;
	I e' un insieme rappresentante il contesto congiuntivo

	*/

	cc(swff(S, 0), [swff(S, 0)]). /* aggiunto nella versione V13 */
	cc(swff(S, 1), [swff(S, 1)]). /* aggiunto nella versione V13 */
	cc(swff(S, F), [swff(S, F)]) :- atom(F).

	/* caso T a&B */
	cc(swff(t, (X & Y)), C) :- cc(swff(t, X), XC), !,
		cc(swff(t, Y), YC), !,
		append(XC, YC, C).

	cc(swff(t, (X <=> Y)), C) :- cc(swff(t, ((X =>Y) & (Y => X))), C), !.

	cc(swff(f, (X | Y)), C) :- cc(swff(f, X), XC), !,
		cc(swff(f, Y), YC), !,
		append(XC, YC, C).

	cc(swff(t, (X | Y)), [swff(t, (X | Y))|C]) :- cc(swff(t, X), XC),
		cc(swff(t, Y), YC),
		intersection(XC, YC, C).

	cc(swff(f, (X & Y)), [swff(f, (X & Y))|C]) :- cc(swff(f, X), XC),
		cc(swff(f, Y), YC),
		intersection(XC, YC, C).

	cc(swff(f, (X <=> Y)), [swff(f, (X <=> Y))|C]) :- cc(swff(f, (X => Y)), XC),
		cc(swff(f, (Y => X)), YC),
		intersection(XC, YC, C).
	cc(swff(t, (X => Y)), [swff(t, (X => Y))]).
	cc(swff(f, (X => Y)), [swff(f, (X => Y))]).

	/*
		Vecchia Regola cc(swff(t,~X),[swff(t,~X)]).
		T~ e' regola che non salta,quindi il suo cc puo' stare con i cc di altre formule.
		Vedi anche commento relativo a pruning.
	*/
	cc(swff(t, ~X), XC) :- cc(swff(fc, X), XC).


	cc(swff(f, ~X), [swff(f, ~X)]).

	cc(swff(fc, (X & Y)), [swff(fc, (X & Y))]).
	cc(swff(fc, (X <=> Y)), [swff(fc, (X <=> Y))]).
	cc(swff(fc, (X | Y)), C) :- cc(swff(fc, X), XC),
		cc(swff(fc, Y), YC),
		append(XC, YC, C).
	cc(swff(fc, (X => Y)), [swff(fc, (X => Y))]).
	cc(swff(fc, ~X), [swff(fc, ~X)]).



	/*

		pruning(Swff,PrunedSwff)

	*/

	pruning(swff(S, 1), swff(S, 1)).
	pruning(swff(S, 0), swff(S, 0)).
	pruning(swff(S, X), swff(S, X)) :- atom(X).

	pruning(swff(t, (X <=> Y)), PRUNED) :- pruning(swff(t, ((X => Y) & (Y => X))), PRUNED).

	pruning(swff(t, (X & Y)), PRUNED) :-
	/*calcola il contesto congiuntivo della
	formula	*/ ccTAnd((X & Y), CCX), !,
		list_to_set(CCX, CCXSET),
		pruningSetSwff(CCXSET, CCXSetPruned),
		simplification(CCXSetPruned, CCXSetPruned, RES), !,
		list_to_set(RES, SET),
	/*
	La formula di input e' di segno T,
	quindi in SET ci devono essere solo
	sottoformule con segno T
	*/ fromSetTotSwff(SET, H),
		/*esegue semplificazioni booleane*/ valSWFF(H, PRUNED).



	pruning(swff(t, (X | Y)), PRUNED) :-
		pruning(swff(t, X), swff(t, HX)), !,
		pruning(swff(t, Y), swff(t, HY)), !,
		cc(swff(t, (HX | HY)), CCX), !,
		list_to_set(CCX, CCXSET),
		simplification(CCXSET, [swff(t, (HX | HY))|CCXSET], RES), !,
		list_to_set(RES, SET),
		fromSetTotSwff(SET, H), !,
		valSWFF(H, PRUNED).


	pruning(swff(f, (X | Y)), PRUNED) :- ccFor((X | Y), CCX), !,
		list_to_set(CCX, CCXSET),
		pruningSetSwff(CCXSET, CCXSetPruned),
		simplification(CCXSetPruned, CCXSetPruned, RES), !,
		list_to_set(RES, SET),
		fromSetTofSwff(SET, H), !,
		valSWFF(H, PRUNED).

	pruning(swff(f, (X <=> Y)), PRUNED) :- pruning(swff(f, ((X => Y) & (Y => X))), PRUNED).

	pruning(swff(f, (X & Y)), PRUNED) :- pruning(swff(f, X), swff(f, HX)), !,
		pruning(swff(f, Y), swff(f, HY)), !,
		cc(swff(f, (HX & HY)), CCX), !,
		list_to_set(CCX, CCXSET),
		simplification(CCXSET, [swff(f, (HX & HY))|CCXSET], RES), !,
		list_to_set(RES, SET),
		fromSetTofSwff(SET, H), !,
		valSWFF(H, PRUNED).


	pruning(swff(f, (X => Y)), PRUNED) :-
		/*semplifica sottoformula sx*/ pruning(swff(t, X), HX), !,
		pruning(swff(f, Y), HY), !,
		/*calcola i contesti congiuntivi*/ cc(HX, CCX), !,
		cc(HY, CCY), !,
		union(CCX, CCY, L),
		list_to_set(L, LSET),
		/*applica la semplificazione*/ simplification(LSET, LSET, RES), !,
		list_to_set(RES, SET),
		/*si prepara a ricostruire la swff*/ convertFcIntoTSWff(SET, SETOK), !,
		estraeTSWff(SETOK, TSwffs),
		subtract(SETOK, TSwffs, FSwffs),
		/*costruisce antecedente*/ fromSetTotSwff(TSwffs, swff(t, TH)), !,
		/*costruisce conseguente*/ fromSetTofSwff(FSwffs, swff(f, FH)), !,
		/*esegue valutazione booleana*/ valSWFF(swff(f, (TH => FH)), PRUNED).

	pruning(swff(t, (X => Y)), PRUNED) :- pruning(swff(f, X), swff(f, HX)), !,
		pruning(swff(t, Y), swff(t, HY)), !,
		valSWFF(swff(t, (HX => HY)), PRUNED).



	pruning(swff(f, ~X), PRUNED) :- pruning(swff(t, X), swff(t, HX)), !,
		valSWFF(swff(f, ~ HX), PRUNED).

	pruning(swff(t, ~X), PRUNED) :- pruning(swff(fc, X), swff(fc, HX)), !,
		valSWFF(swff(t, ~ HX), PRUNED).

	pruning(swff(fc, ~X), PRUNED) :- pruning(swff(t, X), swff(t, HX)), !,
		valSWFF(swff(fc, ~ HX), PRUNED).


	pruning(swff(fc, (X => Y)), PRUNED) :- pruning(swff(t, X), HX), !,
		pruning(swff(fc, Y), HY), !,
		/*calcola i contesti congiuntivi*/ cc(HX, CCX), !,
		cc(HY, CCY), !,
		union(CCX, CCY, L),
		list_to_set(L, LSET),
		/*applica la semplificazione*/ simplification(LSET, LSET, RES), !,
		list_to_set(RES, SET),
		/*si prepara a ricostruire la swff*/ estraeTSWff(SET, TSwffs), !,
		subtract(SET, TSwffs, FcSwffs),
		/*costruisce antecedente*/ fromSetTotSwff(TSwffs, swff(t, TH)), !,
		/*costruisce conseguente*/ fromSetTofcSwff(FcSwffs, swff(fc, FH)), !,
		/*esegue valutazione booleana*/ valSWFF(swff(fc, (TH => FH)), PRUNED).


	pruning(swff(fc, (X | Y)), PRUNED) :- pruning(swff(fc, X), swff(fc, HX)), !,
		pruning(swff(fc, Y), swff(fc, HY)), !,
		cc(swff(fc, (HX | HY)), CCX), !,
		list_to_set(CCX, CCXSET),
		simplification([swff(fc, HX), swff(fc, HY)|CCXSET], [swff(fc, HX), swff(fc, HY)|CCXSET], RES), !,
		list_to_set(RES, SET),
		fromSetTofcSwff(SET, H), !,
		valSWFF(H, PRUNED).

	pruning(swff(fc, (X <=> Y)), PRUNED) :- pruning(swff(fc, ((X => Y) & (Y => X))), PRUNED).

	pruning(swff(fc, (X & Y)), PRUNED) :- pruning(swff(fc, X), swff(fc, HX)), !,
		pruning(swff(fc, Y), swff(fc, HY)), !,
		valSWFF(swff(fc, (HX & HY)), PRUNED).
	/*******************************
	Versione piu' "aggressiva",non so se corretta. Inoltre,bisogna considerare se  vogliamo
	calcolare il cc di un insieme S	di formule. Con l'attuale implementazione di pruningSetSwff la parte qui sotto
	potrebbe essere corretta. Se la fc-wff e' una sottoformula di un albero allora e' sottoformula segnata
	di una formula certa e tutte le formule del contesto congiuntivo a cui essa potenzialemte appartiene sono certe,
	quindi il salto della regola Fc AND non fa sparire formule del contesto congiuntivo

							cc(swff(fc,and(HX,HY)),CCX),!,
							list_to_set(CCX,CCXSET),
							simplification(CCXSET,[swff(f,and(HX,HY))|CCXSET],RES),!,
							list_to_set(RES,SET),
							fromSetTofSwff(SET,H),!,
							valSWFF(H,PRUNED).
	**********************************************			*/


	pruning(X, Y) :- write('chiamata a pruning('), write(X), write(','), write(Y), write(') fallita'), abort.


	estraeTSWff([], []).
	estraeTSWff([swff(t, X)|T], [swff(t, X)|RES]) :- estraeTSWff(T, RES), !.
	estraeTSWff([swff(f, _)|T], RES) :- estraeTSWff(T, RES), !.
	estraeTSWff([swff(fc, _)|T], RES) :- estraeTSWff(T, RES), !. /* caso aggiunto in V13*/
	estraeTSWff(_, _) :- writeln('*** CASO NON PREVISTO IN  estraeTSWff ***'), abort.



	/*fromSetTotSwff([],_):-	writeln('ERRORE IN fromSetTotSwff'),abort.*/

	fromSetTotSwff([], swff(t, 1)). /* modificato in V13 */
	fromSetTotSwff([swff(t, X)], swff(t, X)).
	fromSetTotSwff([swff(fc, X)], swff(t, ~X)). /* caso aggiunto in V13 */
	fromSetTotSwff([swff(t, X)|RES], swff(t, (X & Y))) :- fromSetTotSwff(RES, swff(t, Y)), !.
	fromSetTotSwff([swff(fc, X)|RES], swff(t, (~X & Y))) :- fromSetTotSwff(RES, swff(t, Y)), !. /* caso aggiunto in V13*/
	fromSetTotSwff(X, _) :- write('ERRORE IN fromSetTotSwff('), write(X), write(')'), abort.




	/*fromSetTofSwff([],_):-	writeln('ERRORE IN fromSetTofSwff'),abort.*/

	fromSetTofSwff([], swff(f, 0)).
	fromSetTofSwff([swff(f, X)], swff(f, X)).
	fromSetTofSwff([swff(f, X)|RES], swff(f, (X | Y))) :- fromSetTofSwff(RES, swff(f, Y)), !.
	fromSetTofSwff(X, _) :- write('ERRORE IN fromSetTofSwff('), write(X), write(')'), abort.


	/*fromSetTofcSwff([],_):-	writeln('ERRORE IN fromSetTofSwff'),abort.*/

	fromSetTofcSwff([], swff(fc, 0)).
	fromSetTofcSwff([swff(fc, X)], swff(fc, X)).
	fromSetTofcSwff([swff(fc, X)|RES], swff(fc, (X | Y))) :- fromSetTofcSwff(RES, swff(fc, Y)), !.
	fromSetTofcSwff(X, _) :- write('ERRORE IN fromSetTofSwff('), write(X), write(')'), abort.


	convertFcIntoTSWff([], []).
	convertFcIntoTSWff([swff(fc, X)|T], [swff(t, ~X)|R]) :- convertFcIntoTSWff(T, R), !.
	convertFcIntoTSWff([swff(t, X)|T], [swff(t, X)|R]) :- convertFcIntoTSWff(T, R), !.
	convertFcIntoTSWff([swff(f, X)|T], [swff(f, X)|R]) :- convertFcIntoTSWff(T, R), !.
	convertFcIntoTSWff(X, _) :- write('ERRORE IN convertFcIntoTSWff('), write(X), write(')'), abort.



	/*
		PERMANENZA DEL SEGNO PER FORMULE CLASSICHE
	 */

	/*
					Preso Set ne esegue la semplificazione classica,
					il risultato Ã¨ in NewSet.
	*/

	clSimplification(Set, NewSet) :- buildFalseAtomicSwffs(Set, SetOfFalseAtomicSwffs),
		union(Set, SetOfFalseAtomicSwffs, SimpSet),
		simplification(SetOfFalseAtomicSwffs, SimpSet, NewSet0),
		% Paulo Moura: added the next list_to_set/2 to allow using an ordered set representation
		list_to_set(NewSet0, NewSet).

	/*
						Costruisce le swff atomiche date le variabili con segno costante
						nella parte classica di Set
	*/
	buildFalseAtomicSwffs(Set, SetOfFalseAtomicSwffs) :- clVarsOfASet(Set, Posive, Negative),
		subtract(Negative, Posive, TheFalseVars),
		fromFalseVarsToFalseAtomicSwffs(TheFalseVars, SetOfFalseAtomicSwffs).

	/*				Dato l'insieme a primo argomento calcola le variabili positive e quelle negative
					che compaiono nella parte classica,mettendole a secondo e terzo argomento
	*/
	clVarsOfASet([], [], []).
	clVarsOfASet([A|T], PositiveVars, NegativeVars) :- clVarsInSWFF(A, PosVarsInA, NegVarsInA),
		clVarsOfASet(T, PosVarsInT, NegVarsInT),
		union(PosVarsInA, PosVarsInT, PositiveVars),
		union(NegVarsInA, NegVarsInT, NegativeVars).


	clVarsInSWFF(swff(t, A), PosVarsInA, NegVarsInA) :- clVarsInSWFFTrue(A, PosVarsInA, NegVarsInA),
		!.

	clVarsInSWFF(swff(f, A), [], []) :- atomic(A),
		!.

	clVarsInSWFF(swff(f, A), PosVarsInA, NegVarsInA) :- clVarsInSWFFFalse(A, PosVarsInA, NegVarsInA),
		!.

	clVarsInSWFF(swff(fc, _), [], []).
	/*
						data la swff,mette le variabili classiche positive nell'insieme a
						primo argomento,quelle negative a secondo.
	*/

	clVarsInSWFFTrue(1, [], []).

	clVarsInSWFFTrue(0, [], []).

	clVarsInSWFFTrue(X, [X], []) :- atom(X).


	clVarsInSWFFTrue((X & Y), PosVars, NegVars) :- clVarsInSWFFTrue(X, PVarsX, NVarsX),
		clVarsInSWFFTrue(Y, PVarsY, NVarsY),
		union(PVarsX, PVarsY, PosVars),
		union(NVarsX, NVarsY, NegVars).

	clVarsInSWFFTrue((X <=> Y), PosVars, NegVars) :- clVarsInSWFFTrue(X, PVarsX, NVarsX),
		clVarsInSWFFTrue(Y, PVarsY, NVarsY),
		union(PVarsX, PVarsY, PosVars),
		union(NVarsX, NVarsY, NegVars).

	clVarsInSWFFTrue((X | Y), PosVars, NegVars) :- clVarsInSWFFTrue(X, PVarsX, NVarsX),
		clVarsInSWFFTrue(Y, PVarsY, NVarsY),
		union(PVarsX, PVarsY, PosVars),
		union(NVarsX, NVarsY, NegVars).

	clVarsInSWFFTrue((_ => Y), PosVars, NegVars) :- clVarsInSWFFTrue(Y, PosVars, NegVars).
	clVarsInSWFFTrue(~ _, [], []).


	clVarsInSWFFFalse(1, [], []).
	clVarsInSWFFFalse(swff(f, 0), [], []).
	clVarsInSWFFFalse(X, [], [X]) :- atom(X).

	clVarsInSWFFFalse((X & Y), PosVars, NegVars) :- clVarsInSWFFFalse(X, PVarsX, NVarsX),
		clVarsInSWFFFalse(Y, PVarsY, NVarsY),
		union(PVarsX, PVarsY, PosVars),
		union(NVarsX, NVarsY, NegVars).

	clVarsInSWFFFalse((X | Y), PosVars, NegVars) :- clVarsInSWFFFalse(X, PVarsX, NVarsX),
		clVarsInSWFFFalse(Y, PVarsY, NVarsY),
		union(PVarsX, PVarsY, PosVars),
		union(NVarsX, NVarsY, NegVars).

	clVarsInSWFFFalse((_ => _), [], []).
	clVarsInSWFFFalse((_ <=> _), [], []).
	clVarsInSWFFFalse(~ _, [], []).


	fromFalseVarsToFalseAtomicSwffs([], []). /* Se non ci sono variabili classiche costanti nulla Ã¨ costruito */

	fromFalseVarsToFalseAtomicSwffs([A|T], [swff(f, A)|SetOfFalseAtomicSwffsInT]) :-
		fromFalseVarsToFalseAtomicSwffs(T, SetOfFalseAtomicSwffsInT).


	cercaPermanenceFormula(Set, [TheMainPremise], Resto, [Atom|MoreAtoms]) :-
		partecerta(Set, Sc), /* Sc e' la parte certa di S */
		length(Sc, SizeSc),
		SizeSc > 0,
		subtract(Set, Sc, Sf), /* Sf contiene solo F-formule */
		length(Sf, SizeSf), /* se c'e' solo una F-formula allora */
		SizeSf > 1, /* l'eventuale applicazione di F-> e F-not Ã¨ invertibile */
		prendeTipo3(Sf, Tipo3, _), /*_ contiene solo F-atomic formulas*/
		findMainPremise(Sc, Tipo3, TheMainPremise, [Atom|MoreAtoms]),
		subtract(Set, [TheMainPremise], Resto).

	findMainPremise(Sc, [Swff|_], Swff, TheConstantAtoms) :-
		atomiConSegnoCostanteInSwffSet([Swff|Sc], [Atom|MoreAtoms]),
		atomiConSegnoCostanteInSwffSet([Swff], [SwffAtom|SwffMoreAtoms]),
		intersection([Atom|MoreAtoms], [SwffAtom|SwffMoreAtoms], [HeadAtom|TailAtoms]),
		list_to_set([HeadAtom|TailAtoms], TheConstantAtoms), !.

	findMainPremise(Sc, [_|TailTipo3], Swff, TheConstantAtoms) :- findMainPremise(Sc, TailTipo3, Swff, TheConstantAtoms).


	/*
	Per ogni atomo in Atoms,rimuove l'eventuale opposto in RightSuccessor. Opposti indesiderati vengono
	generati da PermanenzaSegno.
	*/

	rimuoviAtomiOpposti(Atoms, Model, FilteredModel) :- atomiOpposti(Atoms, AtomiOpposti),
		modelloFiltrato(Model, AtomiOpposti, FilteredModel).

	modelloFiltrato([], _, []).

	/*
	*
	*		Versione 2.0
	*
	*/

	modelloFiltrato([swff(_, Wff)|T], AtomiOpposti, FilteredModel) :- \+ atomic(Wff),
		!,
		modelloFiltrato(T, AtomiOpposti, FilteredModel).

	modelloFiltrato([swff(S, Wff)|T], AtomiOpposti, FilteredModel) :- memberchk(swff(S, Wff), AtomiOpposti), !,
		modelloFiltrato(T, AtomiOpposti, FilteredModel).

	modelloFiltrato([swff(S, Wff)|T], AtomiOpposti, [swff(S, Wff)|FilteredModel]) :- modelloFiltrato(T, AtomiOpposti, FilteredModel).

	modelloFiltrato([[X|Y]|Z], AtomiOpposti, [FilteredFirst|FilteredTail]) :-
		!,
		modelloFiltrato([X|Y], AtomiOpposti, FilteredFirst),
		!,
		modelloFiltrato(Z, AtomiOpposti, FilteredTail),
		!.

	modelloFiltrato([[]|Z], AtomiOpposti, [FilteredFirst|FilteredTail]) :-
		modelloFiltrato([], AtomiOpposti, FilteredFirst),
		modelloFiltrato(Z, AtomiOpposti, FilteredTail).


	atomiOpposti([], []).
	atomiOpposti([swff(t, X)|TailAtoms], [swff(fc, X)|TailOpposti]) :- atomiOpposti(TailAtoms, TailOpposti).
	atomiOpposti([swff(fc, X)|TailAtoms], [swff(t, X)|TailOpposti]) :- atomiOpposti(TailAtoms, TailOpposti).


	mainConsequent([], H, [H]).
	mainConsequent([SignedAtom|TailOfAtoms], TheSwff, SetWithTheNewFimplica) :-
		substitute(SignedAtom, [SignedAtom, TheSwff], Res, _),
		checkTheSubstitute(SignedAtom, TailOfAtoms, Res, SetWithTheNewFimplica).

	checkTheSubstitute(SignedAtom, TailOfAtoms, Res, SetWithTheNewFimplica) :-
		subtract(Res, [SignedAtom], [TheNewSwff]), !,
		mainConsequent(TailOfAtoms, TheNewSwff, SetWithTheNewFimplica).

	checkTheSubstitute(_, _, _, []).


	/*equivTable(X,X,1).*/
	equivTable(0, 0, 1).
	equivTable(0, 1, 0).
	equivTable(0, X, ~X).
	equivTable(1, 0, 0).
	equivTable(1, 1, 1).
	equivTable(1, X, X).
	equivTable(X, 0, ~X).
	equivTable(X, 1, X).
	equivTable(X, Y, (X <=> Y)).


	filtraNonAtomiche([], []) :- !.

	/* Nel modello teniamo le atomiche */
	filtraNonAtomiche([swff(S, Wff)|T], [swff(S, Wff)|Result]) :- atomic(Wff),
		!,
		filtraNonAtomiche(T, Result).
	/* Eliminiamo le NON atomiche */
	filtraNonAtomiche([swff(_, _)|T], Result) :- !,
		filtraNonAtomiche(T, Result).


	/* Se arriviamo qui il primo elemento della lista Ã¨ una lista e quindi non filtriamo nulla */
	filtraNonAtomiche(H, H).


	messageOnClosedSet(_). /*:-		writeln('\nBack to the branching point:\n'),
		printSWFFSet(SetToPrint,1),
		writeln(' ').*/


	conservaTatomiche([], []) :- !.

	/* Nel modello teniamo le T-atomiche */
	conservaTatomiche([swff(t, Wff)|T], [swff(t, Wff)|Result]) :- atomic(Wff),
		!,
		conservaTatomiche(T, Result).
	/* Eliminiamo le NON atomiche */
	conservaTatomiche([swff(_, _)|T], Result) :- !,
		conservaTatomiche(T, Result).


	/* Se arriviamo qui il primo elemento della lista Ã¨ una lista e quindi non filtriamo nulla */
	conservaTatomiche(_, _) :- writeln('*** ERRORE IN conservaTatomiche ***'),
		abort.

	estraiListaTAnd((X & Y), ListaAnd) :- !,
		estraiListaTAnd(X, ListaX),
		estraiListaTAnd(Y, ListaY),
		union(ListaX, ListaY, ListaAnd).

	estraiListaTAnd(X, [swff(t, X)]).


	estraiListaTOr((X | Y), ListaAnd) :- !,
		estraiListaTOr(X, ListaX),
		estraiListaTOr(Y, ListaY),
		union(ListaX, ListaY, ListaAnd).

	estraiListaTOr(X, [swff(t, X)]).


	estraiListaFOr((X | Y), ListaOr) :- !,
		estraiListaFOr(X, ListaX),
		estraiListaFOr(Y, ListaY),
		union(ListaX, ListaY, ListaOr).

	estraiListaFOr(X, [swff(f, X)]).

	estraiListaFAnd((X & Y), ListaOr) :- !,
		estraiListaFAnd(X, ListaX),
		estraiListaFAnd(Y, ListaY),
		union(ListaX, ListaY, ListaOr).

	estraiListaFAnd(X, [swff(f, X)]).


	trattaListaTor([swff(t, X)], S, NEWS) :-
		semplificazioneBranch([swff(t, X)], [swff(t, X)|S], NEWS),
		write('Branch of T or,Disjunct: '),
		printWFF(X),
		write(' of ').

	trattaListaTor([swff(t, X), swff(t, Y)|ListaOr], S, NEWS) :-
		(	semplificazioneBranch([swff(t, X)], [swff(t, X)|S], NEWS),
			write('Branch of T or,Disjunct: '),
			printWFF(X),
			write(' of ')
		;	messageOnClosedSet([swff(t, X)|S]),
			trattaListaTor([swff(t, Y)|ListaOr], S, NEWS)
		).


	orderEquiv((X <=> Y), Res) :- orderEquiv(X, ResX),
		orderEquiv(Y, ResY),
		(
			ResX @< ResY,
			!,
			Res = (ResX <=> ResY)
		;
			Res = (ResY <=> ResX)
		).

	orderEquiv((X & Y), Res) :- orderEquiv(X, ResX),
		orderEquiv(Y, ResY),
		(
			ResX @< ResY,
			!,
			Res = (ResX & ResY)
		;
			Res = (ResY & ResX)
		).

	orderEquiv((X | Y), Res) :- orderEquiv(X, ResX),
		orderEquiv(Y, ResY),
		(
			ResX @< ResY,
			!,
			Res = (ResX | ResY)
		;
			Res = (ResY | ResX)
		).

	orderEquiv((X => Y), (ResX => ResY)) :- orderEquiv(X, ResX),
		orderEquiv(Y, ResY).

	orderEquiv(~X, ~ ResX) :- orderEquiv(X, ResX).

	orderEquiv(X, X) :- atom(X).


	orderEquivSet([], []).
	orderEquivSet([swff(Sign, H)|T], [swff(Sign, H1)|T1]) :-
		orderEquiv(H, H1),
		orderEquivSet(T, T1).

	% portability predicates

	:- meta_predicate(time(0)).
	time(Goal) :-
		cpu_time(Time0),
		call(Goal),
		cpu_time(Time1),
		Time is Time1 - Time0,
		write('Time: '), write(Time), write(' seconds'), nl.

	writeln(Term) :-
		write(Term), nl.

	tab(N) :-
		M is N,
		forall(between(1, M, _), put_char(' ')).

	upcase_atom(Atom, UpCaseAtom) :-
		atom_codes(Atom, Codes),
		upcase_atom_(Codes, UpCaseCodes),
		atom_codes(UpCaseAtom, UpCaseCodes).

	upcase_atom_([], []).
	upcase_atom_([Code| Codes], [UpCaseCode| UpCaseCodes]) :-
		(	between(0'a, 0'z, Code) ->
			UpCaseCode is Code - 32
		;	UpCaseCode is Code
		),
		upcase_atom_(Codes,UpCaseCodes).

:- end_object.
