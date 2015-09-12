%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard is/2 built-in predicate.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.1.7

	succeeds(iso_is_2_01) :-
		{X is '+'(7, 35)},
		X == 42.

	succeeds(iso_is_2_02) :-
		{X is '+'(0, 3+11)},
		X == 14.

	succeeds(iso_is_2_03) :-
		{X is '+'(0, 3.2+11)},
		X == 14.2.

	% in some of the throws/2 tests that follow, try to delay the expected error to runtime

	throws(iso_is_2_04, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '+'(77, N)}.

	throws(iso_is_2_05, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '+'(Foo, 77)}.

	succeeds(iso_is_2_06) :-
		{X is '-'(7)},
		X == -7.

	succeeds(iso_is_2_07) :-
		{X is '-'(3-11)},
		X == 8.

	succeeds(iso_is_2_08) :-
		{X is '-'(3.2-11)},
		X == 7.8.

	throws(iso_is_2_09, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '-'(N)}.

	throws(iso_is_2_10, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '-'(Foo)}.

	succeeds(iso_is_2_11) :-
		{X is '-'(7, 35)},
		X == -28.

	succeeds(iso_is_2_12) :-
		{X is '-'(20, 3+11)},
		X == 6.

	succeeds(iso_is_2_13) :-
		{X is '-'(0, 3.2+11)},
		X == -14.2.

	throws(iso_is_2_14, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '-'(77, N)}.

	throws(iso_is_2_15, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '-'(Foo, 77)}.

	succeeds(iso_is_2_16) :-
		{X is '*'(7,35)},
		X == 245.

	succeeds(iso_is_2_17) :-
		{X is '*'(0, 3+11)},
		X == 0.

	succeeds(iso_is_2_18) :-
		{X is '*'(1.5, 3.2+11)},
		X =~= 21.3.

	throws(iso_is_2_19, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '*'(77, N)}.

	throws(iso_is_2_20, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '*'(Foo, 77)}.

	succeeds(iso_is_2_21) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{X is '//'(7,35)},
		X == 0.

	succeeds(iso_is_2_22) :-
		{X is '/'(7.0,35)},
		X == 0.2.

	succeeds(iso_is_2_23) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{X is '//'(140,3+11)},
		X == 10.

	succeeds(iso_is_2_24) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{X is '/'(20.164, 3.2+11)},
		X =~= 1.42.

	succeeds(iso_is_2_25) :-
		{X is '//'(7, -3)},
		(X == -2; X == -3).

	succeeds(iso_is_2_26) :-
		{X is '//'(-7, 3)},
		(X == -2; X == -3).

	throws(iso_is_2_27, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '/'(77, N)}.

	throws(iso_is_2_28, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '/'(Foo, 77)}.

	throws(iso_is_2_29, error(evaluation_error(zero_divisor),_)) :-
		% try to delay the expected error to runtime
		{G = (_X is '/'(3, 0)), call(G)}.

	succeeds(iso_is_2_30) :-
		{X is mod(7, 3)},
		X == 1.

	succeeds(iso_is_2_31) :-
		{X is mod(0, 3+11)},
		X == 0.

	succeeds(iso_is_2_32) :-
		{X is mod(7,-2)},
		X == -1.

	throws(iso_is_2_33, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is mod(77, N)}.

	throws(iso_is_2_34, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is mod(Foo, 77)}.

	throws(iso_is_2_35, error(type_error(integer,7.5),_)) :-
		% try to delay the expected error to runtime
		{G = (_X is mod(7.5, 2)), call(G)}.

	throws(iso_is_2_36, error(evaluation_error(zero_divisor),_)) :-
		% try to delay the expected error to runtime
		{G = (_X is mod(7, 0)), call(G)}.

	succeeds(iso_is_2_37) :-
		{X is floor(7.4)},
		X == 7.

	succeeds(iso_is_2_38) :-
		{X is floor(-0.4)},
		X == -1.

	succeeds(iso_is_2_39) :-
		{X is round(7.5)},
		X == 8.

	succeeds(iso_is_2_40) :-
		{X is round(7.6)},
		X == 8.

	succeeds(iso_is_2_41) :-
		{X is round(-0.6)},
		X == -1.

	throws(iso_is_2_42, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is round(N)}.

	succeeds(iso_is_2_43) :-
		{X is ceiling(-0.5)},
		X == 0.

	succeeds(iso_is_2_44) :-
		{X is truncate(-0.5)},
		X == 0.

	throws(iso_is_2_45, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is truncate(Foo)}.

	succeeds(iso_is_2_46) :-
		{X is float(7)},
		X == 7.0.

	succeeds(iso_is_2_47) :-
		{X is float(7.3)},
		X == 7.3.

	succeeds(iso_is_2_48) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{X is float(5//3)},
		X == 1.0.

	throws(iso_is_2_49, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is float(N)}.

	throws(iso_is_2_50, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is float(Foo)}.

	succeeds(iso_is_2_51) :-
		{X is abs(7)},
		X == 7.

	succeeds(iso_is_2_52) :-
		{X is abs(3-11)},
		X == 8.

	succeeds(iso_is_2_53) :-
		{X is abs(3.2-11.0)},
		X == 7.8.

	throws(iso_is_2_54, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is abs(N)}.

	throws(iso_is_2_55, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is abs(Foo)}.

	:- if(current_prolog_flag(bounded, true)).

	throws(iso_is_2_56, error(evaluation_error(int_overflow),_)) :-
		{current_prolog_flag(max_integer, MI), _X is '+'(MI,1)}.

	throws(iso_is_2_57, error(evaluation_error(int_overflow),_)) :-
		{current_prolog_flag(max_integer, MI), _X is '-'('+'(MI,1),1)}.

	throws(iso_is_2_58, error(evaluation_error(int_overflow),_)) :-
		% ISO allows min_integer = -(max_integer + 1)
		{current_prolog_flag(max_integer, MI), _X is '-'(-2,MI)}.

	throws(iso_is_2_59, error(evaluation_error(int_overflow),_)) :-
		{current_prolog_flag(max_integer, MI), _X is '*'(MI,2)}.

	throws(iso_is_2_60, error(evaluation_error(int_overflow),_)) :-
		{current_prolog_flag(max_integer, MI), R is float(MI)*2, _X is floor(R)}.

	:- else.

	succeeds(iso_is_2_56).

	succeeds(iso_is_2_57).

	succeeds(iso_is_2_58).

	succeeds(iso_is_2_59).

	succeeds(iso_is_2_60).

	:- endif.

	% tests from the Logtalk portability work

	throws(lgt_is_2_61, error(type_error(evaluable,foo/3),_)) :-
		% try to delay the error to runtime
		foo(3, Foo),
		{_X is abs(Foo)}.

	% tests from the ECLiPSe test suite

	succeeds(eclipse_is_2_62) :-
		{X is floor(3.5)},
		X == 3.

	succeeds(eclipse_is_2_63) :-
		{X is ceiling(3.5)},
		X == 4.

	succeeds(eclipse_is_2_64) :-
		{X is truncate(3.5)},
		X == 3.

	succeeds(eclipse_is_2_65) :-
		{X is round(3.5)},
		X == 4.

	succeeds(eclipse_is_2_66) :-
		{X is round(4.5)},
		X == 5.

	succeeds(eclipse_is_2_67) :-
		{X is floor(-3.5)},
		X == -4.

	succeeds(eclipse_is_2_68) :-
		{X is ceiling(-3.5)},
		X == -3.

	succeeds(eclipse_is_2_69) :-
		{X is truncate(-3.5)},
		X == -3.

	succeeds(eclipse_is_2_70) :-
		{X is round(-3.5)},
		X == -3.

	succeeds(eclipse_is_2_71) :-
		{X is round(-4.5)},
		X == -4.

	succeeds(eclipse_is_2_72) :-
		{X = log(9.9), X > 0}.

	succeeds(eclipse_is_2_73) :-
		{X = log(9.9), _ is X + 1}.

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).
	foo(3, foo(1,2,3)).

:- end_object.
