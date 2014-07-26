
:- object(modules_diagram_support).

	:- info([
		version is 0.9,
		author is 'Paulo Moura',
		date is 2014/07/25,
		comment is 'Utility predicates for supporting Prolog modules in diagrams.'
	]).

	:- public(module_property/2).
	:- mode(module_property(?atom, ?callable), zero_or_more).
	:- info(module_property/2, [
		comment is 'Access to module properties, at least exports/1, file/1, and file/2 but also defines/2, calls/2, and provides/3 when possible.',
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
		property_module(declares(Functor/Arity, []), Module) :-
			{module_property(Module, exports(Exports)),
			 member(Functor/Arity, Exports)
			}.
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

		loaded_file_property(File, Property) :-
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

		source_file_extension('.pl').
		source_file_extension('.prolog').
		source_file_extension('.yap').

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		{:- use_module(library(prolog_xref))}.

		module_property(Module, Property) :-
			property_module(Property, Module).

		property_module(exports(Exports), Module) :-
			{module_property(Module, exports(Exports))}.
		property_module(declares(Functor/Arity, []), Module) :-
			{module_property(Module, exports(Exports)),
			 member(Functor/Arity, Exports)
			}.
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

		loaded_file_property(File, Property) :-
			property_source_file(Property, File),
			\+ sub_atom(File, _, 4, 0, '.lgt'),
			\+ sub_atom(File, _, 8, 0, '.logtalk').

		property_source_file(parent(Parent), File) :-
			{source_file_property(File, load_context(user, Parent:_, _)),
			 \+ source_file_property(Parent, derived_from(_))}.
		property_source_file(parent(Parent), File) :-
			{source_file_property(File, load_context(Module, _, _)),
			 module_property(Module, file(Parent))
			}.
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
		property_module(file(File), Module) :-
			{current_module(Module, File)}.
		property_module(file(Basename, Directory), Module) :-
			{current_module(Module, File),
			 decompose_file_name(File, Directory, Name, Extension),
			 atom_concat(Name, Extension, Basename)
			}.

		loaded_file_property(File, Property) :-
			property_source_file(Property, File).

		property_source_file(parent(_Parent), _File) :-
			fail.
		property_source_file(directory(Directory), File) :-
			decompose_file_name(File, Directory, _, _).
		property_source_file(basename(Basename), File) :-
			decompose_file_name(File, _, Name, Extension),
			atom_concat(Name, Extension, Basename).

		decompose_file_name(File, Directory, Name, Extension) :-
			% avoid using 0'Char notation as backend Prolog compilers
			% that don't support it also need to *parse* this code
			char_code('/', SlashCode),
			char_code('.', PeriodCode),
			atom_codes(File, FileCodes),
			(	strrch(FileCodes, SlashCode, [_Slash| BasenameCodes]) ->
				atom_codes(Basename, BasenameCodes),
				atom_concat(Directory, Basename, File)
			;	Directory = './',
				atom_codes(Basename, FileCodes),
				BasenameCodes = FileCodes
			),
			(	strrch(BasenameCodes, PeriodCode, ExtensionCodes) ->
				atom_codes(Extension, ExtensionCodes),
				atom_concat(Name, Extension, Basename)
			;	Name = Basename,
				Extension = ''
			).

		% the following auxiliary predicate was written by Per Mildner and 
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

		source_file_extension('.pl').
		source_file_extension('.prolog').

	:- endif.

:- end_object.
