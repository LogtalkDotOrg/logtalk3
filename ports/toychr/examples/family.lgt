%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% 000401 Slim Abdennadher and Henning Christiansen
%%
%% ported to hProlog by Tom Schrijvers



:- set_logtalk_flag(hook, toychrdb).


:- object(family,
	extends(toychrdb)).

%:- constraints
%  % extensional predicates:
%     person/2, father/2, mother/2,
%     orphan/1,
%  % intensional predicates:
%     parent/2, sibling/2,
%  % predefined:
%     diff/2,
%  % a little helper:
%     start/0.

% Representing the test for failed state, i.e.,
% that the 'predefined' are satisfiable

diff(X,X) ==> false.

chr_option_allow_deep_guards.

% Definition rules:

parent_def @
parent(P,C) <=> (true | (father(P,C) ; mother(P,C))).

sibling_def @
sibling(C1,C2) <=>
         diff(C1,C2),
         parent(P,C1), parent(P,C2).

ext_intro @
start <=> father(john,mary),   father(john,peter),
          mother(jane,mary),
          person(john,male),   person(peter,male),
          person(jane,female), person(mary,female),
          person(paul,male).



% Closing rules
father_close @
father(X,Y) ==> ( true | ((X=john, Y=mary) ; (X=john, Y=peter))).

% mother close @
mother(X,Y) ==> X=jane, Y=mary.

% person_close @
person(X,Y) ==> ( true | ( (X=john, Y=male)   ;
                           (X=peter, Y=male)  ;
                           (X=jane, Y=female) ;
                           (X=mary, Y=female) ;
                           (X=paul, Y=male)
                         )
                ).



% ICs

ic_father_unique @
father(F1,C),father(F2,C) ==> F1=F2.


ic_mother_unique @
mother(M1,C),mother(M2,C) ==> M1=M2.

ic_gender_unique @
person(P,G1), person(P,G2) ==> G1=G2.

ic_father_persons @
father(F,C) ==> person(F,male), person(C,S).

ic_mother_persons @
mother(M,C) ==> person(M,female), person(C,G).

% Indirect def.

orphan1 @
orphan(C) ==>  person(C,G).

orphan2 @
orphan(C), /* person(F,male),*/ father(F,C) ==> false.

orphan3 @
orphan(C), /* person(M,female),*/ mother(M,C) ==> false.



%%%% The following just to simplify output;


father(F,C) \ father(F,C)<=> true.
mother(M,C) \ mother(M,C)<=> true.
person(M,C) \ person(M,C)<=> true.
orphan(C) \ orphan(C)<=> true.


/*************************************************
Sample goals

  :- start, sibling(peter,mary).

  :- start, sibling(paul,mary).

  :- father(X,Y), mother(X,Y).

**************************************************/
:- end_object.
