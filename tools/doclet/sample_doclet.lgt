
:- object(sample_doclet,
	extends(doclet)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2016/08/02,
		comment is 'Example of a doclet object.'
	]).

	% in this example doclet, we automate (re)generating API documentation
	% and an entity diagram for the "lgtunit" tool
	
	% usage is simple: just send the message update/0 to this object
	% (after loading the "lgtdoc" and "diagrams" tools of course)

	% to automatically (re)generate the documentation when this file is
	% loaded, uncomment the following directive:
	%:- initialization(::update).

	% define one clause per goal required to generate the documentation
	% (these goals will be called in the context of "user")
	doc_goal(logtalk_load([lgtunit(loader), lgtdoc(loader), diagrams(loader)])).
	doc_goal(lgtdoc::library(lgtunit, [xml_docs_directory(docs)])).
	doc_goal(entity_diagram::library(lgtunit, [title('Logtalk lgtunit tool'), node_type_captions(true), output_directory(docs)])).

	% define one clause per shell command to be executed
	shell_command('cd docs && lgt2html -t "API documentation for the lgtunit tool"').
	shell_command('cd docs && dot -Tpdf lgtunit_entity_diagram.dot > lgtunit_entity_diagram.pdf').
	shell_command('cd docs && rm -f *.xml && rm -f *.dtd && rm -f *.xsd && rm -f custom.ent && rm -f *.dot').

:- end_object.
