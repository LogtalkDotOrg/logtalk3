%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paul Brown <pbrown@optimusprime.ai> and
%                      Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(html).

	:- info([
		version is 0:2:0,
		author is 'Paul Brown and Paulo Moura',
		date is 2021-03-30,
		comment is 'HTML generation.'
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++term), one_or_error).
	:- info(generate/2, [
		comment is 'Generates HTML content using the representation specified in the first argument (``stream(Stream)`` or ``file(Path)``) for the term in the second argument.',
		argnames is ['Sink', 'Term']
	]).

	:- public(void_element/1).
	:- mode(void_element(?atom), zero_or_more).
	:- info(void_element/1, [
		comment is 'Enumerates, by backtracking, all void elements.',
		argnames is ['Element']
	]).

	:- public(normal_element/2).
	:- mode(normal_element(?atom, ?atom), zero_or_more).
	:- info(normal_element/2, [
		comment is 'Enumerates, by backtracking, all normal elements. The value of the ``Display`` argument is either ``inline`` or ``block``.',
		argnames is ['Element', 'Display']
	]).

	:- private(doctype/1).
	:- mode(doctype(?atom), one).
	:- info(doctype/1, [
		comment is 'Doctype text.',
		argnames is ['DocType']
	]).

	generate(Sink, _) :-
		\+ ground(Sink),
		instantiation_error.
	generate(_, Term) :-
		\+ ground(Term),
		instantiation_error.
	generate(file(File), Term) :-
		open(File, write, Stream),
		write_html(Term, Stream),
		close(Stream),
		!.
	generate(stream(Stream), Term) :-
		write_html(Term, Stream),
		!.
	generate(Sink, _) :-
		domain_error(html_sink, Sink).

	write_html([], _) :-
		!.
	write_html([Fragment| Fragments], Stream) :-
		!,
		write_html(Fragment, Stream),
		write_html(Fragments, Stream).
	write_html(Content, Stream) :-
		atomic(Content),
		!,
		write(Stream, Content).
	write_html(Content, Stream) :-
		write_html_element(Content, Stream).

	write_html_doctype(Stream) :-
		::doctype(Text),
		write(Stream, Text), nl(Stream).

	% handle html/1-2 elements explicitly to output the doctype
	write_html_element(html(Attributes, Content), Stream) :-
		!,
		write_html_doctype(Stream),
		write_html_element(2, html, html(Attributes, Content), Stream).
	write_html_element(html(Content), Stream) :-
		!,
		write_html_doctype(Stream),
		write_html_element(1, html, html(Content), Stream).
	% handle pre/1-2 elements explicitly to escape their content
	write_html_element(pre(Attributes, Content), Stream) :-
		!,
		write_html_open_tag(pre, Attributes, block, Stream),
		write(Stream, Content),
		write_html_close_tag(pre, block, Stream).
	write_html_element(pre(Content), Stream) :-
		!,
		write_html_open_tag(pre, block, Stream),
		write(Stream, Content),
		write_html_close_tag(pre, block, Stream).
	% handle code/1-2 elements explicitly to escape their content
	write_html_element(code(Attributes, Content), Stream) :-
		!,
		write_html_open_tag(code, Attributes, inline, Stream),
		write(Stream, Content),
		write_html_close_tag(code, inline, Stream).
	write_html_element(code(Content), Stream) :-
		!,
		write_html_open_tag(code, inline, Stream),
		write(Stream, Content),
		write_html_close_tag(code, inline, Stream).
	% other elements
	write_html_element(Tag, Stream) :-
		functor(Tag, Name, Arity),
		write_html_element(Arity, Name, Tag, Stream).

	% void element; only attributes allowed
	write_html_element(1, Name, Tag, Stream) :-
		::void_element(Name),
		!,
		arg(1, Tag, Attributes),
		write_html_void_element(Name, Attributes, Stream).
	% normal element with no attributes
	write_html_element(1, Name, Tag, Stream) :-
		::normal_element(Name, Breaks),
		!,
		write_html_open_tag(Name, Breaks, Stream),
		arg(1, Tag, Content),
		write_html(Content, Stream),
		write_html_close_tag(Name, Breaks, Stream).
	% normal element with attributes
	write_html_element(2, Name, Tag, Stream) :-
		::normal_element(Name, Breaks),
		!,
		arg(1, Tag, Attributes),
		arg(2, Tag, Content),
		write_html_open_tag(Name, Attributes, Breaks, Stream),
		write_html(Content, Stream),
		write_html_close_tag(Name, Breaks, Stream).
	% invalid element
	write_html_element(_, _, Tag, _) :-
		domain_error(html_element, Tag).

	write_html_void_element(Name, Attributes, Stream) :-
		write(Stream, '<'), write(Stream, Name),
		write_html_tag_attributes(Attributes, Stream),
		write(Stream, ' />\n').

	write_html_tag_attributes([], _).
	write_html_tag_attributes([Attribute| Attributes], Stream) :-
		write_html_tag_attribute(Attribute, Stream),
		write_html_tag_attributes(Attributes, Stream).
	write_html_tag_attributes(Key=Value, Stream) :-
		write_html_tag_attribute(Key=Value, Stream).
	write_html_tag_attributes(Key-Value, Stream) :-
		write_html_tag_attribute(Key=Value, Stream).

	write_html_tag_attribute(Key=Value, Stream) :-
		write(Stream, ' '),
		write(Stream, Key),
		write(Stream, '="'),
		write(Stream, Value),
		write(Stream, '"').
	write_html_tag_attribute(Key-Value, Stream) :-
		write_html_tag_attribute(Key=Value, Stream).

	write_html_open_tag(Name, Attributes, Breaks, Stream) :-
		write(Stream, '<'), write(Stream, Name),
		write_html_tag_attributes(Attributes, Stream),
		write(Stream, '>'),
		break(Breaks, Stream).

	write_html_open_tag(Name, Breaks, Stream) :-
		write(Stream, '<'), write(Stream, Name), write(Stream, '>'),
		break(Breaks, Stream).

	write_html_close_tag(Name, Breaks, Stream) :-
		break(Breaks, Stream),
		write(Stream, '</'), write(Stream, Name), write(Stream, '>'),
		break(Breaks, Stream).

	break(inline, _).
	break(block, Stream) :-
		nl(Stream).

	void_element(area).
	void_element(base).
	void_element(br).
	void_element(col).
	void_element(embed).
	void_element(hr).
	void_element(img).
	void_element(input).
	void_element(link).
	void_element(meta).
	void_element(param).
	void_element(source).
	void_element(track).
	void_element(wbr).

	normal_element(a, inline).
	normal_element(abbr, inline).
	normal_element(acronym, inline).
	normal_element(address, block).
	normal_element(applet, block).
	normal_element(article, block).
	normal_element(aside, block).
	normal_element(audio, inline).
	normal_element(b, inline).
	normal_element(basefont, block).
	normal_element(bdi, inline).
	normal_element(bdo, inline).
	normal_element(big, inline).
	normal_element(blockquote, block).
	normal_element(body, block).
	normal_element(button, inline).
	normal_element(canvas, inline).
	normal_element(caption, block).
	normal_element(center, block).
	normal_element(cite, inline).
	normal_element(code, inline).
	normal_element(colgroup, block).
	normal_element(data, inline).
	normal_element(datalist, inline).
	normal_element(dd, block).
	normal_element(del, inline).
	normal_element(details, block).
	normal_element(dfn, inline).
	normal_element(dialog, block).
	normal_element(dir, block).
	normal_element(div, block).
	normal_element(dl, block).
	normal_element(dt, block).
	normal_element(em, inline).
	normal_element(fieldset, block).
	normal_element(figcaption, block).
	normal_element(figure, block).
	normal_element(font, inline).
	normal_element(footer, block).
	normal_element(form, block).
	normal_element(frame, block).
	normal_element(frameset, block).
	normal_element(head, block).
	normal_element(header, block).
	normal_element(hgroup, block).
	normal_element(h1, block).
	normal_element(h2, block).
	normal_element(h3, block).
	normal_element(h4, block).
	normal_element(h5, block).
	normal_element(h6, block).
	normal_element(html, block).
	normal_element(i, inline).
	normal_element(iframe, inline).
	normal_element(ins, inline).
	normal_element(kbd, inline).
	normal_element(keygen, inline).
	normal_element(label, inline).
	normal_element(legend, inline).
	normal_element(li, block).
	normal_element(main, block).
	normal_element(map, block).
	normal_element(mark, inline).
	normal_element(menu, block).
	normal_element(menuitem, inline).
	normal_element(meter, inline).
	normal_element(nav, block).
	normal_element(noframes, block).
	normal_element(noscript, inline).
	normal_element(object, inline).
	normal_element(ol, block).
	normal_element(optgroup, block).
	normal_element(option, block).
	normal_element(output, inline).
	normal_element(p, block).
	normal_element(picture, inline).
	normal_element(pre, block).
	normal_element(progress, inline).
	normal_element(q, inline).
	normal_element(rp, inline).
	normal_element(rt, inline).
	normal_element(ruby, inline).
	normal_element(s, inline).
	normal_element(samp, inline).
	normal_element(script, inline).
	normal_element(section, block).
	normal_element(select, inline).
	normal_element(small, inline).
	normal_element(span, inline).
	normal_element(strike, inline).
	normal_element(strong, inline).
	normal_element(style, block).
	normal_element(sub, inline).
	normal_element(summary, block).
	normal_element(sup, inline).
	normal_element(svg, inline).
	normal_element(table, block).
	normal_element(tbody, block).
	normal_element(td, block).
	normal_element(template, inline).
	normal_element(textarea, inline).
	normal_element(tfoot, block).
	normal_element(th, block).
	normal_element(thead, block).
	normal_element(time, inline).
	normal_element(title, block).
	normal_element(tr, block).
	normal_element(tt, inline).
	normal_element(u, inline).
	normal_element(ul, block).
	normal_element(var, inline).
	normal_element(video, inline).

:- end_category.


:- object(html5,
	imports(html)).

	:- info([
		version is 1:0:0,
		author is 'Paul Brown and Paulo Moura',
		date is 2021-03-29,
		comment is 'HTML content generation using the HTML 5 doctype.'
	]).

	doctype('<!DOCTYPE html>').

:- end_object.


:- object(xhtml11,
	imports(html)).

	:- info([
		version is 1:0:0,
		author is 'Paul Brown and Paulo Moura',
		date is 2021-03-29,
		comment is 'XHTML content generation using the XHTML 1.1 doctype.'
	]).

	doctype('<?xml version="1.0" encoding="utf-8" standalone="no"?>\n<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">').

:- end_object.
