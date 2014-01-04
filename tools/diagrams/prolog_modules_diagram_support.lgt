
:- object(prolog_modules_diagram_support).

	:- info([
		version is 0.2,
		author is 'Paulo Moura',
		date is 2014/01/04,
		comment is 'Utility predicates for supporting Prolog modules in diagrams.']).

	:- public(module_property/2).
	:- mode(module_property(?atom, ?callable), zero_or_more).
	:- info(module_property/2, [
		comment is 'Access to module properties, at least exports/1 and file/1.',
		argnames is ['Module', 'Property']]).

	:- public(source_file_property/2).
	:- mode(source_file_property(?atom, ?callable), zero_or_more).
	:- info(source_file_property/2, [
		comment is 'Access to loaded source file properties, at least basename/1, directory/1, and parent/1.',
		argnames is ['File', 'Property']]).

	:- if(current_logtalk_flag(modules, supported)).

		:- if(current_logtalk_flag(prolog_dialect, yap)).

			module_property(Module, Property) :-
				{module_property(Module, Property)}.

			source_file_property(File, Property) :-
				property_source_file(Property, File),
				\+ sub_atom(File, _, 4, 0, '.lgt'),
				\+ sub_atom(File, _, 8, 0, '.logtalk').

			property_source_file(parent(Parent), File) :-
				{source_file_property(File, load_context(Parent0, _, _))},
				(	logtalk::loaded_file_property(Parent, target(Parent0)) ->
					true
				;	Parent = Parent0
				).
			property_source_file(directory(Directory), File) :-
				{source_file(File),
				 file_directory_name(File, Directory0),
				 atom_concat(Directory0, '/', Directory)}.
			property_source_file(basename(Basename), File) :-
				{source_file(File),
				 file_base_name(File, Basename)}.

		:- elif(current_logtalk_flag(prolog_dialect, swi)).

			module_property(Module, Property) :-
				{module_property(Module, Property)}.

			source_file_property(File, Property) :-
				property_source_file(Property, File),
				\+ sub_atom(File, _, 4, 0, '.lgt'),
				\+ sub_atom(File, _, 8, 0, '.logtalk').

			property_source_file(parent(Parent), File) :-
				{source_file_property(File, load_context(user, Parent:_, _)),
				 \+ source_file_property(Parent, derived_from(_))}.
			property_source_file(directory(Directory), File) :-
				{source_file(File),
				 file_directory_name(File, Directory0),
				 atom_concat(Directory0, '/', Directory)}.
			property_source_file(basename(Basename), File) :-
				{source_file(File),
				 file_base_name(File, Basename)}.

		:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

			module_property(Module, Property) :-
				property_module(Property, Module).

			property_module(exports(Exports), Module) :-
				{get_module_info(Module, raw_interface, Interface)},
				filter_interface(Interface, Exports).
			property_module(file(File), Module) :-
				{current_compiled_file(File, _, Module)}.

			filter_interface([], []).
			filter_interface([Functor/Arity| Interface], [Functor/Arity| Exports]) :-
				filter_interface(Interface, Exports).
			filter_interface([op(Priority, Spec, Operators)| Interface], [op(Priority, Spec, Operators)| Exports]) :-
				filter_interface(Interface, Exports).
			filter_interface([export(Functor/Arity)| Interface], [Functor/Arity| Exports]) :-
				!,
				filter_interface(Interface, Exports).
			filter_interface([export(op(Priority, Spec, Operators))| Interface], [op(Priority, Spec, Operators)| Exports]) :-
				!,
				filter_interface(Interface, Exports).
			filter_interface([export(_)| Interface], Exports) :-
				!,
				filter_interface(Interface, Exports).
			filter_interface([_| Interface], Exports) :-
				filter_interface(Interface, Exports).

			source_file_property(File, Property) :-
				property_source_file(Property, File).

			property_source_file(parent(_Parent), _File) :-
				fail.
			property_source_file(directory(Directory), File) :-
				{current_compiled_file(File, _, _),
				 pathname(File, DirectoryString, _, _),
				 atom_string(Directory, DirectoryString)}.
			property_source_file(basename(Basename), File) :-
				{current_compiled_file(File, _, _),
				 pathname(File, _, NameString, ExtensionString),
				 concat_strings(NameString, ExtensionString, BasenameString),
				 atom_string(Basename, BasenameString)}.

		:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

			module_property(Module, Property) :-
				property_module(Property, Module).

			property_module(exports(Exports), Module) :-
				fail.
			property_module(file(File), Module) :-
				{current_module(Module, File)}.

			source_file_property(File, Property) :-
				property_source_file(Property, File).

			property_source_file(parent(_Parent), _File) :-
				fail.
			property_source_file(directory(Directory), File) :-
				decompose_file_name(File, Directory, _, _).
			property_source_file(basename(Basename), File) :-
				decompose_file_name(File, _, Name, Extension),
				atom_concat(Name, Extension, Basename).

			decompose_file_name(File, Directory, Name, Extension) :-
				atom_codes(File, FileCodes),
				(	strrch(FileCodes, 0'/, [_Slash| BasenameCodes]) ->
					atom_codes(Basename, BasenameCodes),
					atom_concat(Directory, Basename, File)
				;	Directory = './',
					atom_codes(Basename, FileCodes),
					BasenameCodes = FileCodes
				),
				(	strrch(BasenameCodes, 0'., ExtensionCodes) ->
					atom_codes(Extension, ExtensionCodes),
					atom_concat(Name, Extension, Basename)
				;	Name = Basename,
					Extension = ''
				).

			% the following auxiliar predicate was written by Per Mildner and 
			% is used here (renamed just to avoid conflicts) with permission
			strrch(Xs, G, Ys) :-
				Xs = [X| Xs1],
				(	X == G ->
					strrch1(Xs1, G, Xs, Ys)
				;	strrch(Xs1, G, Ys)
				).
			
			strrch1(Xs, _G, _Prev, _Ys) :-
				var(Xs),
				!,
				fail.
			strrch1([], _G, Prev, Ys) :-
				Ys = Prev.
			strrch1(Xs, G, Prev, Ys) :-
				Xs = [X| Xs1],
				(	X == G ->
					strrch1(Xs1, G, Xs, Ys)
				;	strrch1(Xs1, G, Prev, Ys)
				).

		:- endif.

	:- endif.

:- end_object.
