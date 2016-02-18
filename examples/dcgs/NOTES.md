________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>

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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

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
