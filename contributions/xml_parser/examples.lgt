/* Using xml.pl to solve XML Query Cases - An Example
 *
 * The following is a complete example to illustrate how the module can be used;
 * it exercises both the input and output parsing modes of xml::parse/[2,3], and
 * illustrates the use of xml::subterm/2 to access the nodes of a "document value
 * model". It's written for Quintus Prolog, but should port to other Prologs
 * easily.
 *
 * The entry-point of the program is the test/1 predicate.
 *
 * test( +QueryId ) executes a Prolog implementation of a Query from Use Case
 * "XMP": Experiences and Exemplars, in the W3C's XML Query Use Cases, which
 * "contains several example queries that illustrate requirements gathered from
 * the database and document communities".
 * <http://www.w3.org/TR/2002/WD-xmlquery-use-cases-20021115/#xmp>
 *
 * QueryId is one of q1…q12 selecting which of the 12 use cases is executed.
 * The XML output is written to the file [QueryId].xml in the current directory.
 *
 * xml::pp/1 is used to display the resulting "document value model"
 % data-structures on the user output (stdout) stream.
 */
test( Query ) :-
	xml_query( Query, ResultElement ),
	% Parse output XML into the Output chars
	xml::parse( Codes, xml([], [ResultElement]) ),
	atom_concat( Query, '.xml', OutputFile ),
	% Write OutputFile from the Output list of chars
	open( OutputFile, write, Output ),
	put_codes( Codes, Output ),
	close( Output ),
	% Pretty print OutputXML
	write( 'Output XML' ), nl,
	xml::pp( xml([], [ResultElement]) ).

/* xml_query( +QueryNo, ?OutputXML ) when OutputXML is an XML Document Value Model
 * produced by running an example taken, identified by QueryNo from the XML Query
 * "XMP" use case.
 */

% Q1: List books published by Addison-Wesley after 1991, including their year and
% title.

xml_query( q1, element(bib, [], Books) ) :-
	element_name( Title, title ),
	element_name( Publisher, publisher ),
	input_document( 'bib.xml', Bibliography ),
	findall(
		element(book, [year=Year], [Title]),
		(
		xml::subterm( Bibliography, element(book, Attributes, Content) ),
		xml::subterm( Content, Publisher ),
		xml::subterm( Publisher, Text ),
		text_value( Text, "Addison-Wesley" ),
		list::member( year=Year, Attributes ),
		number_codes( YearNo, Year ),
		YearNo > 1991,
		xml::subterm( Content, Title )
		),
		Books
	).

% Q2: Create a flat list of all the title-author pairs, with each pair enclosed
% in a "result" element.

xml_query( q2, element(results, [], Results) ) :-
	element_name( Title, title ),
	element_name( Author, author ),
	element_name( Book, book ),
	input_document( 'bib.xml', Bibliography ),
	findall(
		element(result, [], [Title,Author]),
		(
		xml::subterm( Bibliography, Book ),
		xml::subterm( Book, Title ),
		xml::subterm( Book, Author )
		),
		Results
	).

% Q3: For each book in the bibliography, list the title and authors, grouped
% inside a "result" element.

xml_query( q3, element(results, [], Results) ) :-
	element_name( Title, title ),
	element_name( Author, author ),
	element_name( Book, book ),
	input_document( 'bib.xml', Bibliography ),
	findall(
		element(result, [], [Title|Authors]),
		(
		xml::subterm( Bibliography, Book ),
		xml::subterm( Book, Title ),
		findall( Author, xml::subterm(Book, Author), Authors )
		),
		Results
	).

% Q4: For each author in the bibliography, list the author's name and the titles
% of all books by that author, grouped inside a "result" element.

xml_query( q4, element(results, [], Results) ) :-
	element_name( Title, title ),
	element_name( Author, author ),
	element_name( Book, book ),
	input_document( 'bib.xml', Bibliography ),
	findall( Author, xml::subterm(Bibliography, Author), AuthorBag ),
	sort( AuthorBag, Authors ),
	findall(
		element(result, [], [Author|Titles]),
		(
		list::member( Author, Authors ),
		findall( Title, (
			xml::subterm( Bibliography, Book ),
			xml::subterm( Book, Author ),
			xml::subterm( Book, Title )
			),
			Titles
			)
		),
		Results
	).

% Q5: For each book found at both bn.com and amazon.com, list the title of the
% book and its price from each source.

xml_query( q5, element('books-with-prices', [], BooksWithPrices) ) :-
	element_name( Title, title ),
	element_name( Book, book ),
	element_name( Review, entry ),
	input_document( 'bib.xml', Bibliography ),
	input_document( 'reviews.xml', Reviews ),
	findall(
		element('book-with-prices', [], [
			Title,
			element('price-bn',[], BNPrice ),
			element('price-amazon',[], AmazonPrice )
			] ),
		(
		xml::subterm( Bibliography, Book ),
		xml::subterm( Book, Title ),
		xml::subterm( Reviews, Review ),
		xml::subterm( Review, Title ),
		xml::subterm( Book, element(price,_, BNPrice) ),
		xml::subterm( Review, element(price,_, AmazonPrice) )
		),
		BooksWithPrices
	).

% Q6: For each book that has at least one author, list the title and first two
% authors, and an empty "et-al" element if the book has additional authors.

xml_query( q6, element(bib, [], Results) ) :-
	element_name( Title, title ),
	element_name( Author, author ),
	element_name( Book, book ),
	input_document( 'bib.xml', Bibliography ),
	findall(
		element(book, [], [Title,FirstAuthor|Authors]),
		(
		xml::subterm( Bibliography, Book ),
		xml::subterm( Book, Title ),
		findall( Author, xml::subterm(Book, Author), [FirstAuthor|Others] ),
		other_authors( Others, Authors )
		),
		Results
	).

% Q7: List the titles and years of all books published by Addison-Wesley after
% 1991, in alphabetic order.

xml_query( q7, element(bib, [], Books) ) :-
	element_name( Title, title ),
	element_name( Publisher, publisher ),
	input_document( 'bib.xml', Bibliography ),
	findall(
		Title-element(book, [year=Year], [Title]),
		(
		xml::subterm( Bibliography, element(book, Attributes, Book) ),
		xml::subterm( Book, Publisher ),
		xml::subterm( Publisher, Text ),
		text_value( Text, "Addison-Wesley" ),
		list::member( year=Year, Attributes ),
		number_codes( YearNo, Year ),
		YearNo > 1991,
		xml::subterm( Book, Title )
		),
		TitleBooks
	),
	keysort( TitleBooks, TitleBookSet ),
	range( TitleBookSet, Books ).

% Q8: Find books in which the name of some element ends with the string "or" and
% the same element contains the string "Suciu" somewhere in its content. For each
% such book, return the title and the qualifying element.

xml_query( q8, element(bib, [], Books) ) :-
	element_name( Title, title ),
	element_name( Book, book ),
	element_name( QualifyingElement, QualifyingName ),
	list::append( "Suciu", _Back, Suffix ),
	input_document( 'bib.xml', Bibliography ),
	findall(
		element(book, [], [Title,QualifyingElement]),
		(
		xml::subterm( Bibliography, Book ),
		xml::subterm( Book, QualifyingElement ),
		atom_codes( QualifyingName, QNChars ),
		list::append( _QNPrefix, "or", QNChars ),
		xml::subterm( QualifyingElement, TextItem ),
		text_value( TextItem, TextValue ),
		list::append( _Prefix, Suffix, TextValue ),
		xml::subterm( Book, Title )
		),
		Books
	).

% Q9: In the document "books.xml", find all section or chapter titles that
% contain the word "XML", regardless of the level of nesting.

xml_query( q9, element(results, [], Titles) ) :-
	element_name( Title, title ),
	list::append( "XML", _Back, Suffix ),
	input_document( 'books.xml', Books ),
	findall(
		Title,
		(
		xml::subterm( Books, Title ),
		xml::subterm( Title, TextItem ),
		text_value( TextItem, TextValue ),
		list::append( _Prefix, Suffix, TextValue )
		),
		Titles
	).

% Q10: In the document "prices.xml", find the minimum price for each book, in the
% form of a "minprice" element with the book title as its title attribute.

xml_query( q10, element(results, [], MinPrices) ) :-
	element_name( Title, title ),
	element_name( Price, price ),
	input_document( 'prices.xml', Prices ),
	findall( Title, xml::subterm(Prices, Title), TitleBag ),
	sort( TitleBag, TitleSet ),
	element_name( Book, book ),
	findall(
		element(minprice, [title=TitleString], [MinPrice]),
		(
		list::member( Title, TitleSet ),
		xml::subterm( Title, TitleText ),
		text_value( TitleText, TitleString ),
		findall( PriceValue-Price, (
			xml::subterm( Prices, Book ),
			xml::subterm( Book, Title ),
			xml::subterm( Book, Price ),
			xml::subterm( Price, Text ),
			text_value( Text, PriceChars ),
			number_codes( PriceValue, PriceChars )
			),
			PriceValues
			),
		minimum( PriceValues, PriceValue-MinPrice )
		),
		MinPrices
	).

% Q11: For each book with an author, return the book with its title and authors.
% For each book with an editor, return a reference with the book title and the
% editor's affiliation.

xml_query( q11, element(bib, [], Results) ) :-
	element_name( Title, title ),
	element_name( Author, author ),
	element_name( Book, book ),
	element_name( Editor, editor ),
	element_name( Affiliation, affiliation ),
	input_document( 'bib.xml', Bibliography ),
	findall(
		element(book, [], [Title,FirstAuthor|Authors]),
		(
		xml::subterm( Bibliography, Book ),
		xml::subterm( Book, Title ),
		findall( Author, xml::subterm(Book, Author), [FirstAuthor|Authors] )
		),
		Books
	),
	findall(
		element(reference, [], [Title,Affiliation]),
		(
		xml::subterm( Bibliography, Book ),
		xml::subterm( Book, Title ),
		xml::subterm( Book, Editor ),
		xml::subterm( Editor, Affiliation )
		),
		References
	),
	list::append( Books, References, Results ).

% Q12: Find pairs of books that have different titles but the same set of authors
% (possibly in a different order).

xml_query( q12, element(bib, [], Pairs) ) :-
	element_name( Author, author ),
	element_name( Book1, book ),
	element_name( Book2, book ),
	element_name( Title1, title ),
	element_name( Title2, title ),
	input_document( 'bib.xml', Bibliography ),
	findall(
		element('book-pair', [], [Title1,Title2]),
		(
		xml::subterm( Bibliography, Book1 ),
		findall( Author, xml::subterm(Book1, Author), AuthorBag1 ),
		sort( AuthorBag1, AuthorSet ),
		xml::subterm( Bibliography, Book2 ),
		Book2 @< Book1,
		findall( Author, xml::subterm(Book2, Author), AuthorBag2 ),
		sort( AuthorBag2, AuthorSet ),
		xml::subterm( Book1, Title1 ),
		xml::subterm( Book2, Title2 )
		),
		Pairs
	).

% Auxilliary Predicates

other_authors( [], [] ).
other_authors( [Author|Authors], [Author|EtAl] ) :-
	et_al( Authors, EtAl ).

et_al( [], [] ).
et_al( [_|_], [element('et-al',[],[])] ).

text_value( [pcdata(Text)], Text ).
text_value( [cdata(Text)], Text ).

element_name( element(Name, _Attributes, _Content), Name ).


/* range( +Pairs, ?Range ) when Pairs is a list of key-datum pairs and Range
 * is the list of data.
 */
range( [], [] ).
range( [_Key-Datum|Pairs], [Datum|Data] ) :-
	range( Pairs, Data ).

/* minimum( +List, ?Min ) is true if Min is the least member of List in the
 * standard order.
 */
minimum( [H|T], Min ) :-
	minimum1( T, H, Min ).

minimum1( [], Min, Min ).
minimum1( [H|T], Min0, Min ) :-
	compare( Relation, H, Min0 ),
	minimum2( Relation, H, Min0, T, Min ).

minimum2( '=', Min0, Min0, T, Min ) :-
	minimum1( T, Min0, Min ).
minimum2( '<', Min0, _Min1, T, Min ) :-
	minimum1( T, Min0, Min ).
minimum2( '>', _Min0, Min1, T, Min ) :-
	minimum1( T, Min1, Min ).

/* input_document( +File, ?XML ) reads File and parses the input into the
 * "Document Value Model" XML.
 */
input_document( File, XML ) :-
	% Read InputFile as a list of chars
	open( File, read, Input ),
	get_codes( Input, Codes ),
	close( Input ),
	% Parse the Input chars into the term XML
	xml::parse( Codes, XML ).


put_codes( [], _ ).
put_codes( [Code|Codes], Output ) :-
	put_code( Output, Code ),
	put_codes( Codes, Output ).


get_codes( Input, [Code|Codes] ) :-
	\+ at_end_of_stream( Input ),
	get_code( Input, Code ),
	get_codes( Input, Codes ).
get_codes( _, [] ).
