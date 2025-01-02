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


This folder contains a simple Logtalk language definition for the LaTeX
package `listings`:

http://www.ctan.org/tex-archive/macros/latex/contrib/listings/

To use the language definition copy the `lstlang0.sty` to the same
folder where you have the LaTeX source files that you're typesetting
and add the following lines to your main LaTeX file:

	\usepackage{listings}
	\usepackage{textcomp}

The `textcomp` package is required as the Logtalk language definition
sets the `upquote` option to true.

If you want keywords in bold face in the code listings, you will also
need to use fonts that support that face. You can use e.g. *one* of the
following font packages:

	\usepackage[scaled=0.8]{couriers}
	\usepackage{pxfonts}
	\usepackage{lmodern}
	\usepackage{txfonts}

Default options for all code listings can be set using something like:

	\lstset{
		language=Logtalk,
		basicstyle=\small\ttfamily,
		keywordstyle=\bfseries,
		tabsize=4,
		numbers=none, numberstyle=\tiny, stepnumber=1, numbersep=5pt,
		showspaces=false, showstringspaces=false,
		captionpos=b,
		frame=lines,
		upquote=true,
		framextopmargin=6pt, framexbottommargin=6pt,
		aboveskip=\medskipamount, lineskip={-2.0pt}
	}

If you need to highlight additional keywords, you can always use the
`morekeywords` option as an alternative to edit the `lstlang0.sty`
file. For example:

	morekeywords={extends_object, implements_protocol}

For inline code use `\lstinline{...}` and for code blocks use:

	\begin{lstlisting}[language=Logtalk, ...]
		...
	\end{lstlisting}

Consult the `listings` package documentation (e.g., by running the command
`texdoc listings`) and experiment with the above settings to get the exact
appearance that your publication requires.
