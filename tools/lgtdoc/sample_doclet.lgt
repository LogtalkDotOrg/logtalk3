
:- object(sample_doclet,
	extends(doclet)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/07/29,
		comment is 'Example of a doclet object.'
	]).

	% in this example doclet, we automate (re)generating API documentation
	% for the "lgtdoc" tool; usage is simple: just send the message update/0
	% to this object (after loading the "lgtdoc" tool of course)

	% define one clause per goal required to generate the documentation
	% this goal will be called in the context of "user"
	doc_goal(lgtdoc::library(lgtdoc, [xml_docs_directory(docs)])).

	% define one clause per shell command to be executed
	shell_command('cd docs && lgt2html -t "API documentation for the lgtdoc tool"').

:- end_object.
