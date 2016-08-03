
:- object(doclet(_Library),
	extends(doclet)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/08/03,
		comment is 'Example of a parameterized doclet.',
		parnames is ['Library']
	]).

	% load the documenting tools
	doc_goal(logtalk_load([lgtdoc(loader), diagrams(loader)])).
	% load the application assuming a "loader.lgt" file
	doc_goal(logtalk_load(Loader)) :-
		parameter(1, Library),
		Loader =.. [Library, loader].
	% generate the library API documentation
	doc_goal(lgtdoc::rlibrary(Library, [xml_docs_directory(DocsPath)])) :-
		parameter(1, Library),
		docs_path(DocsPath).
	% generate the diagrams
	doc_goal(diagrams::rlibrary(Library, [title(Library), node_type_captions(true), output_directory(DocsPath)])) :-
		parameter(1, Library),
		docs_path(DocsPath).

	% convert the documentation to the final formats
	shell_command(Command) :-
		cd_docs(CD),
		atom_concat(CD, ' && lgt2html -t "API documentation"', Command).
	shell_command(Command) :-
		cd_docs(CD),
		atom_concat(CD, ' && for f in *.dot; do dot -Tsvg "$f" > "${f%.*}.svg"; done', Command).
	% clean up documentation intermediate files
	shell_command(Command) :-
		cd_docs(CD),
		atom_concat(CD, ' && rm -f *.xml && rm -f *.dtd && rm -f *.xsd && rm -f custom.ent && rm -f *.dot', Command).

	% auxiliary predicates

	docs_path(DocsPath) :-
		parameter(1, Library),
		logtalk::expand_library_path(Library, Path),
		atom_concat(Path, docs, DocsPath).

	cd_docs(CD) :-
		docs_path(DocsPath),
		atom_concat('cd ', DocsPath, CD).		

:- end_object.
