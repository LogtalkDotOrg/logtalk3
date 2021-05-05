:- use_module(library(jpl)).

:- use_module(library(lists)).

:- use_module(library(gensym)).

:- use_module(library(apply)).

:- use_module(library(readutil)).

:- use_module(library(porter_stem)).

:- use_module(library(listing)).

:- use_module(library(ctypes)).

:- use_module(library(error)).

:- set_prolog_flag(double_quotes,codes).

:- object(swi).

:- uses(user,[add_import_module/3,atom_number/2,flag/3,nb_delete/1,nb_getval/2,nb_setval/2,writeln/1,writeln/2]).

:- public[errmes/2].

:- dynamic expand_query/4.

:- end_object.

