
:- use_module(library(pce)).

:- pce_begin_class(name_asker, dialog, "Modal prompter for a name").

initialise(W, Label:[name]) :->
	"Initialise the window and fill it"::
	send_super(W, initialise(Label)),
	send(W, append(text_item(name))),
	send(W, append(button(ok))),
	send(W, append(button(cancel))),
	send(W, default_button(ok)).
        
ok(W) :->
	"User pressed the OK button"::
	get(W, member(name), NameItem),
	get(NameItem, selection, Typed),
	send(W, return, Typed).

cancel(W) :->
	"User pressed the Cancel button"::
	send(W, return(@nil)).

prompt(W, Value:name) :<-
	"Open it, destroy it and return the result"::
	get(W, confirm, Value),
	free(W).

:- pce_end_class.
