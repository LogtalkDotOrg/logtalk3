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


:- object(stopwords_ca,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the ca language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-ca',
			'Source commit' - '222bb5691f90586016c1a3a8342e199ce7b3e399',
			'Generated entries' - '278.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('a').
	stop_word('abans').
	stop_word('ací').
	stop_word('ah').
	stop_word('així').
	stop_word('això').
	stop_word('al').
	stop_word('aleshores').
	stop_word('algun').
	stop_word('alguna').
	stop_word('algunes').
	stop_word('alguns').
	stop_word('alhora').
	stop_word('allà').
	stop_word('allí').
	stop_word('allò').
	stop_word('als').
	stop_word('altra').
	stop_word('altre').
	stop_word('altres').
	stop_word('amb').
	stop_word('ambdues').
	stop_word('ambdós').
	stop_word('anar').
	stop_word('ans').
	stop_word('apa').
	stop_word('aquell').
	stop_word('aquella').
	stop_word('aquelles').
	stop_word('aquells').
	stop_word('aquest').
	stop_word('aquesta').
	stop_word('aquestes').
	stop_word('aquests').
	stop_word('aquí').
	stop_word('baix').
	stop_word('bastant').
	stop_word('bé').
	stop_word('cada').
	stop_word('cadascuna').
	stop_word('cadascunes').
	stop_word('cadascuns').
	stop_word('cadascú').
	stop_word('com').
	stop_word('consegueixo').
	stop_word('conseguim').
	stop_word('conseguir').
	stop_word('consigueix').
	stop_word('consigueixen').
	stop_word('consigueixes').
	stop_word('contra').
	stop_word('d''un').
	stop_word('d''una').
	stop_word('d''unes').
	stop_word('d''uns').
	stop_word('dalt').
	stop_word('de').
	stop_word('del').
	stop_word('dels').
	stop_word('des').
	stop_word('des de').
	stop_word('després').
	stop_word('dins').
	stop_word('dintre').
	stop_word('donat').
	stop_word('doncs').
	stop_word('durant').
	stop_word('e').
	stop_word('eh').
	stop_word('el').
	stop_word('elles').
	stop_word('ells').
	stop_word('els').
	stop_word('em').
	stop_word('en').
	stop_word('encara').
	stop_word('ens').
	stop_word('entre').
	stop_word('era').
	stop_word('erem').
	stop_word('eren').
	stop_word('eres').
	stop_word('es').
	stop_word('esta').
	stop_word('estan').
	stop_word('estat').
	stop_word('estava').
	stop_word('estaven').
	stop_word('estem').
	stop_word('esteu').
	stop_word('estic').
	stop_word('està').
	stop_word('estàvem').
	stop_word('estàveu').
	stop_word('et').
	stop_word('etc').
	stop_word('ets').
	stop_word('fa').
	stop_word('faig').
	stop_word('fan').
	stop_word('fas').
	stop_word('fem').
	stop_word('fer').
	stop_word('feu').
	stop_word('fi').
	stop_word('fins').
	stop_word('fora').
	stop_word('gairebé').
	stop_word('ha').
	stop_word('han').
	stop_word('has').
	stop_word('haver').
	stop_word('havia').
	stop_word('he').
	stop_word('hem').
	stop_word('heu').
	stop_word('hi').
	stop_word('ho').
	stop_word('i').
	stop_word('igual').
	stop_word('iguals').
	stop_word('inclòs').
	stop_word('ja').
	stop_word('jo').
	stop_word('l''hi').
	stop_word('la').
	stop_word('les').
	stop_word('li').
	stop_word('li''n').
	stop_word('llarg').
	stop_word('llavors').
	stop_word('m''he').
	stop_word('ma').
	stop_word('mal').
	stop_word('malgrat').
	stop_word('mateix').
	stop_word('mateixa').
	stop_word('mateixes').
	stop_word('mateixos').
	stop_word('me').
	stop_word('mentre').
	stop_word('meu').
	stop_word('meus').
	stop_word('meva').
	stop_word('meves').
	stop_word('mode').
	stop_word('molt').
	stop_word('molta').
	stop_word('moltes').
	stop_word('molts').
	stop_word('mon').
	stop_word('mons').
	stop_word('més').
	stop_word('n''he').
	stop_word('n''hi').
	stop_word('ne').
	stop_word('ni').
	stop_word('no').
	stop_word('nogensmenys').
	stop_word('només').
	stop_word('nosaltres').
	stop_word('nostra').
	stop_word('nostre').
	stop_word('nostres').
	stop_word('o').
	stop_word('oh').
	stop_word('oi').
	stop_word('on').
	stop_word('pas').
	stop_word('pel').
	stop_word('pels').
	stop_word('per').
	stop_word('per que').
	stop_word('perquè').
	stop_word('però').
	stop_word('poc').
	stop_word('poca').
	stop_word('pocs').
	stop_word('podem').
	stop_word('poden').
	stop_word('poder').
	stop_word('podeu').
	stop_word('poques').
	stop_word('potser').
	stop_word('primer').
	stop_word('propi').
	stop_word('puc').
	stop_word('qual').
	stop_word('quals').
	stop_word('quan').
	stop_word('quant').
	stop_word('que').
	stop_word('quelcom').
	stop_word('qui').
	stop_word('quin').
	stop_word('quina').
	stop_word('quines').
	stop_word('quins').
	stop_word('què').
	stop_word('s''ha').
	stop_word('s''han').
	stop_word('sa').
	stop_word('sabem').
	stop_word('saben').
	stop_word('saber').
	stop_word('sabeu').
	stop_word('sap').
	stop_word('saps').
	stop_word('semblant').
	stop_word('semblants').
	stop_word('sense').
	stop_word('ser').
	stop_word('ses').
	stop_word('seu').
	stop_word('seus').
	stop_word('seva').
	stop_word('seves').
	stop_word('si').
	stop_word('sobre').
	stop_word('sobretot').
	stop_word('soc').
	stop_word('solament').
	stop_word('sols').
	stop_word('som').
	stop_word('son').
	stop_word('sons').
	stop_word('sota').
	stop_word('sou').
	stop_word('sóc').
	stop_word('són').
	stop_word('t''ha').
	stop_word('t''han').
	stop_word('t''he').
	stop_word('ta').
	stop_word('tal').
	stop_word('també').
	stop_word('tampoc').
	stop_word('tan').
	stop_word('tant').
	stop_word('tanta').
	stop_word('tantes').
	stop_word('te').
	stop_word('tene').
	stop_word('tenim').
	stop_word('tenir').
	stop_word('teniu').
	stop_word('teu').
	stop_word('teus').
	stop_word('teva').
	stop_word('teves').
	stop_word('tinc').
	stop_word('ton').
	stop_word('tons').
	stop_word('tot').
	stop_word('tota').
	stop_word('totes').
	stop_word('tots').
	stop_word('un').
	stop_word('una').
	stop_word('unes').
	stop_word('uns').
	stop_word('us').
	stop_word('va').
	stop_word('vaig').
	stop_word('vam').
	stop_word('van').
	stop_word('vas').
	stop_word('veu').
	stop_word('vosaltres').
	stop_word('vostra').
	stop_word('vostre').
	stop_word('vostres').
	stop_word('érem').
	stop_word('éreu').
	stop_word('és').
	stop_word('éssent').
	stop_word('últim').
	stop_word('ús').

:- end_object.
