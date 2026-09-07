:- encoding('UTF-8').

/*
The MIT License (MIT)

Copyright (c) 2016 Gene Diaz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/


:- object(stopwords_yo,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the yo language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-yo',
			'Source commit' - 'ab33b417b40b2677c035a790da8d3e87e9174166',
			'Generated entries' - '60.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('a').
	stop_word('an').
	stop_word('bá').
	stop_word('bí').
	stop_word('bẹ̀rẹ̀').
	stop_word('fún').
	stop_word('fẹ́').
	stop_word('gbogbo').
	stop_word('inú').
	stop_word('jù').
	stop_word('jẹ').
	stop_word('jẹ́').
	stop_word('kan').
	stop_word('kì').
	stop_word('kí').
	stop_word('kò').
	stop_word('láti').
	stop_word('lè').
	stop_word('lọ').
	stop_word('mi').
	stop_word('mo').
	stop_word('máa').
	stop_word('mọ̀').
	stop_word('ni').
	stop_word('náà').
	stop_word('ní').
	stop_word('nígbà').
	stop_word('nítorí').
	stop_word('nǹkan').
	stop_word('o').
	stop_word('padà').
	stop_word('pé').
	stop_word('púpọ̀').
	stop_word('pẹ̀lú').
	stop_word('rẹ̀').
	stop_word('sì').
	stop_word('sí').
	stop_word('sínú').
	stop_word('ṣ').
	stop_word('ti').
	stop_word('tí').
	stop_word('wà').
	stop_word('wá').
	stop_word('wọn').
	stop_word('wọ́n').
	stop_word('yìí').
	stop_word('àti').
	stop_word('àwọn').
	stop_word('é').
	stop_word('í').
	stop_word('òun').
	stop_word('ó').
	stop_word('ń').
	stop_word('ńlá').
	stop_word('ṣe').
	stop_word('ṣé').
	stop_word('ṣùgbọ́n').
	stop_word('ẹmọ́').
	stop_word('ọjọ́').
	stop_word('ọ̀pọ̀lọpọ̀').

:- end_object.
