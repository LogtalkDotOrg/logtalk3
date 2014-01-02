
:- object(prolog_modules_diagram_support).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2014/01/02,
		comment is 'Utility predicates for supporting Prolog modules in diagrams.']).

	:- public(module_property/2).
	:- mode(module_property(?atom, ?callable), zero_or_more).
	:- info(module_property/2, [
		comment is 'Description',
		argnames is ['Module', 'Property']]).

	:- public(source_file_property/2).
	:- mode(source_file_property(?atom, ?callable), zero_or_more).
	:- info(source_file_property/2, [
		comment is 'Description',
		argnames is ['File', 'Property']]).

	:- if(current_logtalk_flag(modules, supported)).

		:- if(current_logtalk_flag(prolog_dialect, swi)).

			module_property(Module, Property) :-
				{module_property(Module, Property)}.

			source_file_property(File, Property) :-
				property_source_file(Property, File),
				\+ sub_atom(File, _, 4, 0, '.lgt'),
				\+ sub_atom(File, _, 8, 0, '.logtalk').

			property_source_file(parent(Parent), File) :-
				{source_file_property(File, load_context(user, Parent:_, _)),
				 \+ source_file_property(Parent, derived_from(_))
				}.
			property_source_file(directory(Directory), File) :-
				{source_file(File),
				 file_directory_name(File, Directory0),
				 atom_concat(Directory0, '/', Directory)
				}.
			property_source_file(basename(Basename), File) :-
				{source_file(File),
				 file_base_name(File, Basename)
				}.

		:- endif.

	:- endif.

:- end_object.
