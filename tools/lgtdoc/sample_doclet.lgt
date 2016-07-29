
:- object(sample_doclet,
	extends(doclet)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/07/29,
		comment is 'Sample doclet object for the "lgtdoc" tool.'
	]).

	doc_goal(lgtdoc::library(lgtdoc, [xml_docs_directory(sample_docs)])).

	shell_command('cd sample_docs && lgt2html -t "Sample API docs for the lgtdoc tool" && cd ..').

:- end_object.
