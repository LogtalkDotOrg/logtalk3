
:- object(modules_diagram_support).

	:- info([
		version is 0.14,
		author is 'Paulo Moura',
		date is 2017/08/10,
		comment is 'Utility predicates for supporting Prolog modules in diagrams.'
	]).

	:- public(module_property/2).
	:- mode(module_property(?atom, ?callable), zero_or_more).
	:- info(module_property/2, [
		comment is 'Access to module properties, at least exports/1, file/1, and file/2 but also declares/2, defines/2, calls/2, and provides/3 when possible.',
		argnames is ['Module', 'Property']
	]).

	:- public(loaded_file_property/2).
	:- mode(loaded_file_property(?atom, ?callable), zero_or_more).
	:- info(loaded_file_property/2, [
		comment is 'Access to loaded source file properties, at least basename/1, directory/1 but also parent/1 when possible.',
		argnames is ['File', 'Property']
	]).

	:- public(source_file_extension/1).
	:- mode(source_file_extension(?atom), one_or_more).
	:- info(source_file_extension/1, [
		comment is 'Valid source file extension for Prolog source files.',
		argnames is ['Extension']
	]).

	:- if(current_logtalk_flag(prolog_dialect, yap)).

		{:- use_module(library(lists))}.
		{:- use_module(library(prolog_xref))}.

		module_property(Module, Property) :-
			property_module(Property, Module).

		property_module(exports(Exports), Module) :-
			{module_property(Module, exports(Exports))}.
		property_module(declares(Functor/Arity, Properties), Module) :-
			{module_property(Module, exports(Exports)),
			 member(Functor/Arity, Exports)
			},
			module_predicate_properties(Module, Functor/Arity, Properties).
		property_module(defines(Functor/Arity, []), Module) :-
			{module_property(Module, file(File)),
			 xref_source(File),
			 setof(How, xref_defined(File, Predicate, How), _),
			 Predicate \= ':'(_,_),
			 \+ xref_defined(File, Predicate, imported(_)),
			 functor(Predicate, Functor, Arity)
			}.
		property_module(provides(Functor/Arity, To, []), Module) :-
			{module_property(Module, file(File)),
			 xref_source(File),
			 setof(Location, xref_defined(File, To:Predicate, local(Location)), _),
			 To \== Module,
			 functor(Predicate, Functor, Arity)
			}.
		property_module(file(File), Module) :-
			{module_property(Module, file(File))}.
		property_module(file(Basename, Directory), Module) :-
			{module_property(Module, file(File)),
			 file_directory_name(File, Directory0),
			 atom_concat(Directory0, '/', Directory),
			 file_base_name(File, Basename)			
			}.
		property_module(calls(Callee, [caller(Caller)]), Module) :-
			{module_property(Module, file(File)),
			 xref_source(File),
			 xref_called(File, Callee0, Caller0),
			 (	Caller0 = ':'(ForModule,Caller1) ->
			 	functor(Caller1, CallerFunctor, CallerArity),
				Caller = ':'(ForModule,CallerFunctor/CallerArity)
			 ;	functor(Caller0, CallerFunctor, CallerArity),
			 	Caller = CallerFunctor/CallerArity
			 ),
			 functor(Caller, CallerFunctor, CallerArity),
			 (	Callee0 = Object::Callee1 ->
			 	functor(Callee1, CalleeFunctor, CalleeArity),
				Callee = Object::CalleeFunctor/CalleeArity
			 ;	Callee0 = ':'(OtherModule,Callee1) ->
			 	functor(Callee1, CalleeFunctor, CalleeArity),
				Callee = ':'(OtherModule,CalleeFunctor/CalleeArity)
			 ;	xref_defined(File, Callee0, imported(FromFile)) ->
			 	once(module_property(FromModule, file(FromFile))),
			 	functor(Callee0, CalleeFunctor, CalleeArity),
			 	Callee = ':'(FromModule,CalleeFunctor/CalleeArity)
			 ;	% assume local predicate
			 	functor(Callee0, CalleeFunctor, CalleeArity),
			 	Callee = CalleeFunctor/CalleeArity
			 )
			}.

		module_predicate_properties(Module, Functor/Arity, Properties) :-
			functor(Predicate, Functor, Arity),
			(	{predicate_property(':'(Module,Predicate), static)} ->
				Properties = [static| Properties0]
			;	Properties = [(dynamic)| Properties0]
			),
			(	{predicate_property(':'(Module,Predicate), (multifile))} ->
				Properties0 = [(multifile)]
			;	Properties0 = []
			).

		loaded_file_property(File, Property) :-
			property_source_file(Property, File),
			os::decompose_file_name(File, _, _, Extension),
			\+ logtalk::file_type_extension(logtalk, Extension).

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

		source_file_extension('.pl').
		source_file_extension('.prolog').
		source_file_extension('.yap').

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		{:- use_module(library(prolog_xref))}.

		module_property(Module, Property) :-
			property_module(Property, Module).

		property_module(exports(Exports), Module) :-
			{module_property(Module, exports(Exports))}.
		property_module(declares(Functor/Arity, Properties), Module) :-
			{module_property(Module, exports(Exports)),
			 member(Functor/Arity, Exports)
			},
			module_predicate_properties(Module, Functor/Arity, Properties).
		property_module(defines(Functor/Arity, []), Module) :-
			{module_property(Module, file(File)),
			 xref_source(File),
			 setof(How, xref_defined(File, Predicate, How), _),
			 Predicate \= ':'(_,_),
			 \+ xref_defined(File, Predicate, imported(_)),
			 functor(Predicate, Functor, Arity)
			}.
		property_module(provides(Functor/Arity, To, []), Module) :-
			{module_property(Module, file(File)),
			 xref_source(File),
			 setof(Location, xref_defined(File, To:Predicate, local(Location)), _),
			 To \== Module,
			 functor(Predicate, Functor, Arity)
			}.
		property_module(file(File), Module) :-
			{module_property(Module, file(File))}.
		property_module(file(Basename, Directory), Module) :-
			{module_property(Module, file(File)),
			 file_directory_name(File, Directory0),
			 atom_concat(Directory0, '/', Directory),
			 file_base_name(File, Basename)			
			}.
		property_module(calls(Callee, [caller(Caller)]), Module) :-
			{module_property(Module, file(File)),
			 xref_source(File),
			 xref_called(File, Callee0, Caller0),
			 (	Caller0 = ':'(ForModule,Caller1) ->
			 	functor(Caller1, CallerFunctor, CallerArity),
				Caller = ':'(ForModule,CallerFunctor/CallerArity)
			 ;	functor(Caller0, CallerFunctor, CallerArity),
			 	Caller = CallerFunctor/CallerArity
			 ),
			 (	Callee0 = Object::Callee1 ->
			 	functor(Callee1, CalleeFunctor, CalleeArity),
				Callee = Object::CalleeFunctor/CalleeArity
			 ;	Callee0 = ':'(OtherModule,Callee1) ->
			 	functor(Callee1, CalleeFunctor, CalleeArity),
				Callee = ':'(OtherModule,CalleeFunctor/CalleeArity)
			 ;	xref_defined(File, Callee0, imported(FromFile)) ->
			 	once(module_property(FromModule, file(FromFile))),
			 	functor(Callee0, CalleeFunctor, CalleeArity),
			 	Callee = ':'(FromModule,CalleeFunctor/CalleeArity)
			 ;	% assume local predicate
			 	functor(Callee0, CalleeFunctor, CalleeArity),
			 	Callee = CalleeFunctor/CalleeArity
			 )
			}.

		module_predicate_properties(Module, Functor/Arity, Properties) :-
			functor(Predicate, Functor, Arity),
			(	{predicate_property(':'(Module,Predicate), (dynamic))} ->
				Properties = [(dynamic)| Properties0]
			;	Properties = [static| Properties0]
			),
			(	{predicate_property(':'(Module,Predicate), (multifile))} ->
				Properties0 = [(multifile)]
			;	Properties0 = []
			).

		loaded_file_property(File, Property) :-
			property_source_file(Property, File).

		property_source_file(parent(Parent), File) :-
			{source_file_property(File0, load_context(user, Parent:_, _))},
			(	{source_file_property(File0, derived_from(File))} ->
				true
			;	File = File0
			).
		property_source_file(parent(Parent), File) :-
			{source_file_property(File, load_context(Module, _, _)),
			 module_property(Module, file(Parent))}.
		property_source_file(parent(Parent), File) :-
			{source_file_property(File, derived_from(Parent, _))}.
		property_source_file(directory(Directory), File) :-
			{source_file(File),
			 file_directory_name(File, Directory0),
			 atom_concat(Directory0, '/', Directory)}.
		property_source_file(basename(Basename), File) :-
			{source_file(File),
			 file_base_name(File, Basename)}.

		source_file_extension('.pl').
		source_file_extension('.prolog').

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		module_property(Module, Property) :-
			property_module(Property, Module).

		property_module(exports(Exports), Module) :-
			{get_module_info(Module, raw_interface, Interface)},
			filter_interface(Interface, Exports).
		property_module(declares(Functor/Arity, Properties), Module) :-
			property_module(exports(Exports), Module),
			list::member(Functor/Arity, Exports),
			module_predicate_properties(Module, Functor/Arity, Properties).
		property_module(defines(Functor/Arity, []), Module) :-
			{'@'(current_module_predicate(defined,Functor/Arity), Module)}.
		property_module(file(File), Module) :-
			{current_compiled_file(File, _, Module)}.
		property_module(file(Basename, Directory), Module) :-
			{current_compiled_file(File, _, Module),
			 pathname(File, DirectoryString, NameString, ExtensionString),
			 atom_string(Directory, DirectoryString),
			 concat_strings(NameString, ExtensionString, BasenameString),
			 atom_string(Basename, BasenameString)
			}.

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

		module_predicate_properties(Module, Functor/Arity, Properties) :-
			(	{'@'(get_flag(Functor/Arity, stability, (dynamic)), Module)} ->
				Properties = [(dynamic)]
			;	{'@'(get_flag(Functor/Arity, stability, static), Module)} ->
				Properties = [static]
			;	Properties = []
			).

		loaded_file_property(File, Property) :-
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

		source_file_extension('.pl').
		source_file_extension('.prolog').

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		module_property(Module, Property) :-
			property_module(Property, Module).

		property_module(exports(Exports), Module) :-
			{findall(
				Functor/Arity,
				(predicate_property(Module:Goal, exported), functor(Goal, Functor, Arity)),
				Exports
			)}.
		property_module(declares(Functor/Arity, Properties), Module) :-
			property_module(exports(Exports), Module),
			list::member(Functor/Arity, Exports),
			module_predicate_properties(Module, Functor/Arity, Properties).
		property_module(defines(Functor/Arity, []), Module) :-
			{current_predicate(_, ':'(Module,Predicate))},
			functor(Predicate, Functor, Arity).
		property_module(file(File), Module) :-
			{current_module(Module, File)}.
		property_module(file(Basename, Directory), Module) :-
			{current_module(Module, File)},
			os::decompose_file_name(File, Directory, Basename).

		module_predicate_properties(Module, Functor/Arity, Properties) :-
			functor(Predicate, Functor, Arity),
			(	{predicate_property(':'(Module,Predicate), (dynamic))} ->
				Properties = [(dynamic)| Properties0]
			;	Properties = [static| Properties0]
			),
			(	{predicate_property(':'(Module,Predicate), (multifile))} ->
				Properties0 = [(multifile)]
			;	Properties0 = []
			).

		loaded_file_property(File, Property) :-
			property_source_file(Property, File).

		property_source_file(parent(_Parent), _File) :-
			fail.
		property_source_file(directory(Directory), File) :-
			{source_file(File)},
			os::decompose_file_name(File, Directory, _).
		property_source_file(basename(Basename), File) :-
			{source_file(File)},
			os::decompose_file_name(File, _, Basename).

		source_file_extension('.pl').
		source_file_extension('.prolog').

	:- endif.

:- end_object.
