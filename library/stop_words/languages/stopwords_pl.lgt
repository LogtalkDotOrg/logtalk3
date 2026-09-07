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


:- object(stopwords_pl,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the pl language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-pl',
			'Source commit' - '723fe9b896c5acf35d1d5cc725f012ce2309291f',
			'Generated entries' - '329.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('a').
	stop_word('aby').
	stop_word('ach').
	stop_word('acz').
	stop_word('aczkolwiek').
	stop_word('aj').
	stop_word('albo').
	stop_word('ale').
	stop_word('ależ').
	stop_word('ani').
	stop_word('aż').
	stop_word('bardziej').
	stop_word('bardzo').
	stop_word('bez').
	stop_word('bo').
	stop_word('bowiem').
	stop_word('by').
	stop_word('byli').
	stop_word('bym').
	stop_word('bynajmniej').
	stop_word('być').
	stop_word('był').
	stop_word('była').
	stop_word('było').
	stop_word('były').
	stop_word('będzie').
	stop_word('będą').
	stop_word('cali').
	stop_word('cała').
	stop_word('cały').
	stop_word('chce').
	stop_word('choć').
	stop_word('ci').
	stop_word('ciebie').
	stop_word('cię').
	stop_word('co').
	stop_word('cokolwiek').
	stop_word('coraz').
	stop_word('coś').
	stop_word('czasami').
	stop_word('czasem').
	stop_word('czemu').
	stop_word('czy').
	stop_word('czyli').
	stop_word('często').
	stop_word('daleko').
	stop_word('dla').
	stop_word('dlaczego').
	stop_word('dlatego').
	stop_word('do').
	stop_word('dobrze').
	stop_word('dokąd').
	stop_word('dość').
	stop_word('dr').
	stop_word('dużo').
	stop_word('dwa').
	stop_word('dwaj').
	stop_word('dwie').
	stop_word('dwoje').
	stop_word('dzisiaj').
	stop_word('dziś').
	stop_word('gdy').
	stop_word('gdyby').
	stop_word('gdyż').
	stop_word('gdzie').
	stop_word('gdziekolwiek').
	stop_word('gdzieś').
	stop_word('go').
	stop_word('godz').
	stop_word('hab').
	stop_word('i').
	stop_word('ich').
	stop_word('ii').
	stop_word('iii').
	stop_word('ile').
	stop_word('im').
	stop_word('inna').
	stop_word('inne').
	stop_word('inny').
	stop_word('innych').
	stop_word('inż').
	stop_word('iv').
	stop_word('ix').
	stop_word('iż').
	stop_word('ja').
	stop_word('jak').
	stop_word('jakaś').
	stop_word('jakby').
	stop_word('jaki').
	stop_word('jakichś').
	stop_word('jakie').
	stop_word('jakiś').
	stop_word('jakiż').
	stop_word('jakkolwiek').
	stop_word('jako').
	stop_word('jakoś').
	stop_word('je').
	stop_word('jeden').
	stop_word('jedna').
	stop_word('jednak').
	stop_word('jednakże').
	stop_word('jedno').
	stop_word('jednym').
	stop_word('jedynie').
	stop_word('jego').
	stop_word('jej').
	stop_word('jemu').
	stop_word('jest').
	stop_word('jestem').
	stop_word('jeszcze').
	stop_word('jeśli').
	stop_word('jeżeli').
	stop_word('już').
	stop_word('ją').
	stop_word('każdy').
	stop_word('kiedy').
	stop_word('kierunku').
	stop_word('kilka').
	stop_word('kilku').
	stop_word('kimś').
	stop_word('kto').
	stop_word('ktokolwiek').
	stop_word('ktoś').
	stop_word('która').
	stop_word('które').
	stop_word('którego').
	stop_word('której').
	stop_word('który').
	stop_word('których').
	stop_word('którym').
	stop_word('którzy').
	stop_word('ku').
	stop_word('lat').
	stop_word('lecz').
	stop_word('lub').
	stop_word('ma').
	stop_word('mają').
	stop_word('mam').
	stop_word('mamy').
	stop_word('mało').
	stop_word('mgr').
	stop_word('mi').
	stop_word('miał').
	stop_word('mimo').
	stop_word('między').
	stop_word('mnie').
	stop_word('mną').
	stop_word('mogą').
	stop_word('moi').
	stop_word('moim').
	stop_word('moja').
	stop_word('moje').
	stop_word('może').
	stop_word('możliwe').
	stop_word('można').
	stop_word('mu').
	stop_word('musi').
	stop_word('my').
	stop_word('mój').
	stop_word('na').
	stop_word('nad').
	stop_word('nam').
	stop_word('nami').
	stop_word('nas').
	stop_word('nasi').
	stop_word('nasz').
	stop_word('nasza').
	stop_word('nasze').
	stop_word('naszego').
	stop_word('naszych').
	stop_word('natomiast').
	stop_word('natychmiast').
	stop_word('nawet').
	stop_word('nic').
	stop_word('nich').
	stop_word('nie').
	stop_word('niech').
	stop_word('niego').
	stop_word('niej').
	stop_word('niemu').
	stop_word('nigdy').
	stop_word('nim').
	stop_word('nimi').
	stop_word('nią').
	stop_word('niż').
	stop_word('no').
	stop_word('nowe').
	stop_word('np').
	stop_word('nr').
	stop_word('o').
	stop_word('o.o.').
	stop_word('obok').
	stop_word('od').
	stop_word('ok').
	stop_word('około').
	stop_word('on').
	stop_word('ona').
	stop_word('one').
	stop_word('oni').
	stop_word('ono').
	stop_word('oraz').
	stop_word('oto').
	stop_word('owszem').
	stop_word('pan').
	stop_word('pana').
	stop_word('pani').
	stop_word('pl').
	stop_word('po').
	stop_word('pod').
	stop_word('podczas').
	stop_word('pomimo').
	stop_word('ponad').
	stop_word('ponieważ').
	stop_word('powinien').
	stop_word('powinna').
	stop_word('powinni').
	stop_word('powinno').
	stop_word('poza').
	stop_word('prawie').
	stop_word('prof').
	stop_word('przecież').
	stop_word('przed').
	stop_word('przede').
	stop_word('przedtem').
	stop_word('przez').
	stop_word('przy').
	stop_word('raz').
	stop_word('razie').
	stop_word('roku').
	stop_word('również').
	stop_word('sam').
	stop_word('sama').
	stop_word('się').
	stop_word('skąd').
	stop_word('sobie').
	stop_word('sobą').
	stop_word('sposób').
	stop_word('swoje').
	stop_word('są').
	stop_word('ta').
	stop_word('tak').
	stop_word('taka').
	stop_word('taki').
	stop_word('takich').
	stop_word('takie').
	stop_word('także').
	stop_word('tam').
	stop_word('te').
	stop_word('tego').
	stop_word('tej').
	stop_word('tel').
	stop_word('temu').
	stop_word('ten').
	stop_word('teraz').
	stop_word('też').
	stop_word('to').
	stop_word('tobie').
	stop_word('tobą').
	stop_word('toteż').
	stop_word('totobą').
	stop_word('trzeba').
	stop_word('tu').
	stop_word('tutaj').
	stop_word('twoi').
	stop_word('twoim').
	stop_word('twoja').
	stop_word('twoje').
	stop_word('twym').
	stop_word('twój').
	stop_word('ty').
	stop_word('tych').
	stop_word('tylko').
	stop_word('tym').
	stop_word('tys').
	stop_word('tzw').
	stop_word('tę').
	stop_word('u').
	stop_word('ul').
	stop_word('vi').
	stop_word('vii').
	stop_word('viii').
	stop_word('vol').
	stop_word('w').
	stop_word('wam').
	stop_word('wami').
	stop_word('was').
	stop_word('wasi').
	stop_word('wasz').
	stop_word('wasza').
	stop_word('wasze').
	stop_word('we').
	stop_word('według').
	stop_word('wie').
	stop_word('wiele').
	stop_word('wielu').
	stop_word('więc').
	stop_word('więcej').
	stop_word('wszyscy').
	stop_word('wszystkich').
	stop_word('wszystkie').
	stop_word('wszystkim').
	stop_word('wszystko').
	stop_word('wtedy').
	stop_word('www').
	stop_word('wy').
	stop_word('właśnie').
	stop_word('wśród').
	stop_word('xi').
	stop_word('xii').
	stop_word('xiii').
	stop_word('xiv').
	stop_word('xv').
	stop_word('z').
	stop_word('za').
	stop_word('zapewne').
	stop_word('zawsze').
	stop_word('zaś').
	stop_word('ze').
	stop_word('zeznowu').
	stop_word('znowu').
	stop_word('znów').
	stop_word('został').
	stop_word('zł').
	stop_word('żaden').
	stop_word('żadna').
	stop_word('żadne').
	stop_word('żadnych').
	stop_word('że').
	stop_word('żeby').

:- end_object.
