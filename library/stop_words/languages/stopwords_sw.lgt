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


:- object(stopwords_sw,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the sw language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-sw',
			'Source commit' - '624fd53c78bd0adee14d798014590f24b1ec8c54',
			'Generated entries' - '74.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('akasema').
	stop_word('alikuwa').
	stop_word('alisema').
	stop_word('baada').
	stop_word('basi').
	stop_word('bila').
	stop_word('cha').
	stop_word('chini').
	stop_word('hadi').
	stop_word('hapo').
	stop_word('hata').
	stop_word('hivyo').
	stop_word('hiyo').
	stop_word('huku').
	stop_word('huo').
	stop_word('ili').
	stop_word('ilikuwa').
	stop_word('juu').
	stop_word('kama').
	stop_word('karibu').
	stop_word('katika').
	stop_word('kila').
	stop_word('kima').
	stop_word('kisha').
	stop_word('kubwa').
	stop_word('kutoka').
	stop_word('kuwa').
	stop_word('kwa').
	stop_word('kwamba').
	stop_word('kwenda').
	stop_word('kwenye').
	stop_word('la').
	stop_word('lakini').
	stop_word('mara').
	stop_word('mdogo').
	stop_word('mimi').
	stop_word('mkubwa').
	stop_word('mmoja').
	stop_word('moja').
	stop_word('muda').
	stop_word('mwenye').
	stop_word('na').
	stop_word('naye').
	stop_word('ndani').
	stop_word('ng').
	stop_word('ni').
	stop_word('nini').
	stop_word('nonkungu').
	stop_word('pamoja').
	stop_word('pia').
	stop_word('sana').
	stop_word('sasa').
	stop_word('sauti').
	stop_word('tafadhali').
	stop_word('tena').
	stop_word('tu').
	stop_word('vile').
	stop_word('wa').
	stop_word('wakati').
	stop_word('wake').
	stop_word('walikuwa').
	stop_word('wao').
	stop_word('watu').
	stop_word('wengine').
	stop_word('wote').
	stop_word('ya').
	stop_word('yake').
	stop_word('yangu').
	stop_word('yao').
	stop_word('yeye').
	stop_word('yule').
	stop_word('za').
	stop_word('zaidi').
	stop_word('zake').

:- end_object.
