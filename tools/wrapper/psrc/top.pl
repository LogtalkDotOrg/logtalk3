% top.pl

% example for run_dynamics
% '$run'(assert(boo(42))).

% toploop in xtop.pl

% main: entry point ??
% main:-toptest,fail.
% main:-toploop,fail;halt.

:-['lib.pl'].
:-['lists.pl'].

:-['shared.pl'].
:-['cserver.pl'].

:-['engine.pl'].

:-['bagof.pl'].
:-['sort.pl'].

:-['xlib.pl'].
:-['xtop.pl'].
:-['xmeta.pl'].

:-['xbuiltins.pl'].
:-['jlib.pl'].

:-['xrefl.pl']. % general reflection

:-['xnet.pl']. % networking

:-['xtask.pl']. % multitasking, threads

:-['xbs.pl'].

% compiler
% :-['../compiler/leanco.pl'].
:-['builtins.pl'].
:-['comp.pl'].

:-['lean.pl'].
% :-['../progs/ltests.pl'].

:-['db.pl'].

:-['xio.pl'].

:-['lean_io.pl'].

