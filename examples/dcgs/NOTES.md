---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

# dcgs

This folder contains the following examples of using DCGs in objects and
categories:

- `calculator`  
	canonical DCG example of parsing arithmetic expressions
- `enigma`  
	solve a cellphone enigma against a dictionary of words
- `bom`  
	bill of materials DCG example (see below for original source)
- `sentences`  
	simple parsing of natural language sentences
- `parsetree`  
	same as above but building and returning the parse tree
- `xml`  
	conversion between XML and Prolog terms
- `url`  
	parsing of URLs, decomposing them in components
- `shell`  
	parsing of command-line shell commands
- `faa`  
	command language DCG example (see below for original source)
- `walker`  
	parsing of walker movements and calculation of distance
	travelled
- `bypass`  
	using the `{}/1` DCG construct together with the `{}/1` Logtalk control 
	construct
- `tokenizer`  
	natural language tokenizer example
- `macaddr`  
	validator for MAC hardware addresses
- `morse`  
	decoder for Morse code messages; illustrate how to use scope 
	directives to declare grammar rule non-terminals
- `iban`  
	IBAN validation; this example can generate very large integers during
	validation and thus may not work with all backend Prolog compilers
- `lambdas`  
	example of using lambda expressions in grammar rules

This folder includes an example, `tokenizer`, adapted with permission from 
a Michael A. Covington example:

http://www.ai.uga.edu/~mc/

See the file `tokenizer.lgt` for more details.

This folder also includes two examples of DCGs, `bom` and `faa`, adapted
with permission from the Amzi! Prolog documentation. The documentation is 
available on-line in HTML format at the URL:

http://www.amzi.com/

Please refer to the Amzi! Prolog documentation for more information on the 
original examples.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(dcgs(loader)).
```

DCG rules implementing a simple calculator:

```logtalk
calculator::parse("1+2-3*4", Result).
```

<!--
Result = -9.
-->

Recognize MAC addresses:

```logtalk
macaddr::valid("00:1e:4a:ef:72:8b").
```

<!--
true.
-->

Decode Morse code messages (as `phrase/2-3` are private methods, we use the
`logtalk` built-in object and the `(<<)/2` control construct to invoke the
`foo//0` public grammar rule non-terminal as the backend Prolog system
implementation of the `phrase/2-3` predicates is not Logtalk aware and
would mishandle the `(::)/2` message-sending operator):

```logtalk
logtalk << phrase(morse::morse(Message), "... --- ...").
```

<!--
Message = [sos].
-->

Solve a cellphone keypad encoded enigma:

```logtalk
enigma::solve("4 96853 5683 86 4283 346637 9484 968 8664448", Message).
```

<!--
Message = [i, would, love, to, have, dinner, with, you, tonight].
-->

Recognizing grammatically correct sentences:

```logtalk
sentence::parse([the, girl, likes, the, boy], Result).
```

<!--
Result = true.
-->

```logtalk
sentence::parse([the, girl, scares, the, boy], Result).
```

<!--
Result = false.
-->

Generating parse trees for sentences:

```logtalk
parsetree::parse([the, girl, likes, the, boy], Tree).
```

<!--
Tree = s(np(d(the), n(girl)), vp(v(likes), np(d(the), n(boy)))).
-->

Bill of materials example:

```logtalk
bom::parts(bike, L).
```

<!--
L = [frame, crank, pedal, pedal, chain, spokes, rim, hub, spokes, rim, hub].
-->

```logtalk
bom::parts(wheel, L).
```

<!--
L = [spokes, rim, hub].
-->

Parsing command-line shell input:

```logtalk
shell::parse("pwd; cd ..; ls -a", L).
```

<!--
L = [pwd,'cd ..','ls -a'].
-->

Convert a string to a list of tokens (words, numbers, punctuation):

```logtalk
tokenizer::tokens(" We owe $1,048,576.24 to Agent 007 for Version 3.14159! ", Tokens).
```

<!--
Tokens = [we,owe,$,1048576.24,to,agent,7,for,version,3.14159,!].
-->

Walker movements:

```logtalk
walker::walk([n(5), e(4), s(2), nw(8), s(5), se(1), n(4)], Ending).
```

<!--
Ending = -0.94974746830583223,6.9497474683058318.
-->

Conversion between compound terms and XML:

```logtalk
xml::convert(word(child, children), word(singular, plural), XML).
```

<!--
XML = '<word><singular>child</singular><plural>children</plural></word>'.
-->

```logtalk
xml::convert(Term, Interpretation, '<word><singular>child</singular><plural>children</plural></word>').
```

<!--
Term = word(child, children), Interpretation = word(singular, plural).
-->

Parsing URLs:

```logtalk
url::parse("https://logtalk.org", Components).
```

<!--
Components = [protocol(http), address([logtalk, org]), path([]), file('')].
-->

```logtalk
url::parse("https://logtalk.org/", Components).
```

<!--
Components = [protocol(http), address([logtalk, org]), path(['']), file('')].
-->

```logtalk
url::parse("https://logtalk.org/cvs", Components).
```

<!--
Components = [protocol(http), address([logtalk, org]), path([cvs]), file('')].
-->

```logtalk
url::parse("https://logtalk.org/cvs.html", Components).
```

<!--
Components = [protocol(http), address([logtalk, org]), path([]), file('cvs.html')].
-->

```logtalk
url::parse("http://193.136.64.5/files/update", Components).
```

<!--
Components = [protocol(http), address([193, 136, 64, 5]), path([files, update]), file('')].
-->

Command language example (skip if running as a notebook):

```logtalk
(current_object(jupyter) -> true; faa::main).
```

<!--
Fly Amzi! Air
enter command> list flights
aa101
aa102
aa103
enter command> book elana aa102
enter command> book tom aa102
enter command> list passengers aa102
elana
tom
enter command> exit

true.
-->

IBAN validation:

```logtalk
iban::valid("GB82 WEST 1234 5698 7654 32").
```

<!--
true.
-->

Double bypass using the `{}/1` control constructs of grammar rules and Logtalk
(as `phrase/2-3` are private methods, we use the `logtalk` built-in object and the
`(<<)/2` control construct to invoke the `foo//0` public grammar rule non-terminal):

```logtalk
logtalk << phrase(bypass::foo, _, _).
```

<!--
bar predicate called.
-->

Run the Logtalk DCG translator on the test cases:

```logtalk
dcgtest::run.
```

Try the `meta_non_terminal/1` + `call//N` examples:

```logtalk
client::print.
```

<!--
1-one
2-two
3-three
a-one
b-two
c-three

true.
-->

```logtalk
client::successors([1,2,3], Successors).
```

<!--
Successors = [2, 3, 4].
-->

Use lambda expressions in grammar rules:

```logtalk
logtalk << phrase(lambdas::aa(Duplicates), [a,b,c]).
```

<!--
Duplicates = [a,a,b,b,c,c].
-->

```logtalk
logtalk << phrase(lambdas::aa([a,a,b,b,c,c]), Singletons).
```

<!--
Singletons = [a,b,c].
-->

Use `call//1` and a lambda expressions to access the grammar rule input list
for debugging:

```logtalk
debug::copy([1,2,3,4,5], Copy).
```

<!--
[1,2,3,4,5]
[2,3,4,5]
[3,4,5]
[4,5]
[5]
[]
Copy = [1, 2, 3, 4, 5].
-->

Use a meta-non-terminal:

```logtalk
logtalk << phrase(meta_nt::repeat(bases::octal, 2, 4, Digits), [3,7,4,1,0,3,6], Rest).
```

<!--
Digits = [3,7,4,1], Rest = [0,3,6].
-->
