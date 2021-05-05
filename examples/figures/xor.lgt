/*-------------------------------------------------------------*/
/*   Connectionist-like NCL implementation of the XOR function */
/*   (C) 1993 Zdravko Markov, II-BAS                           */
/*-------------------------------------------------------------*/
/*
   This is a simple illustration of the spreading activation
mechanism in NCL. The classical connectionist example for the
XOR (exclusive-OR) function is used. The NCL program is a direct
implementation of the corresponding neural network if we assume
that a bound net-variable gives weight of connection 1 and a free
one - weight of connection -1. Then the spreading activation
nodes perform combining function summation and threshold function
unit step.
*/

/*-------------------------------------------------------------*/
/* XOR Inputs */
in1(X): in2(Y):

/* Hidden Layer */
node(X,~Y,1,x1(X)):
node(Y,~X,1,y1(Y)):

/* Output Layer */
x1(X1): y1(Y1):
node(X1,Y1,1,out(on)):
out(Z).
/*-------------------------------------------------------------*/
/* EXAMPLES:                                                   */
/*-------------------------------------------------------------*/
?- netmode(0).  /* Breadth-first mode only ! */
/*
?- top(T),in1(on),spread(T),out(X).     1 XOR 0 = 1
X=on

?- top(T),in1(on),in2(on),spread(T),out(X).   1 XOR 1 = 0
X=_1

*/
