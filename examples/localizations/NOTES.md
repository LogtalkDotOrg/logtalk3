---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.1'
      jupytext_version: 1.16.6
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

# localizations

This folder contains an example of using the message printing mechanism
to provide software localization in several natural languages. Each language
localization is defined in its own category. Typically, only the category
for the selected language would be loaded, possibly with the help of
conditional compilation directives. But we load them all in this example.
The selected language can be represented in several ways. Here we use a
single object parameter. Another alternative would be to use any mechanism
for setting the default language (e.g., a simple global fact). The main
point is that an application core logic should be decoupled from the
natural language used when interacting with the user. Customizing the
application for another natural language should ideally be as simple as
defining a set of entities holding the message text translations and
loading these localization files at application startup. The solution
used in this example support loading more than one localization at the
same time but this is usually not required. In the simple case where
a single localization would be loaded at any given time, the code can be
simplified by removing the country code parametrization from the core
logic.

Load the example:

```logtalk
logtalk_load(localizations(loader)).
```

Print the application language in several languages:

```logtalk
my_game(de)::banner.
```

<!--
>>> Willkommen Sie bei Mein tolles Spiel!
true.
-->

```logtalk
my_game(en)::banner.
```

<!--
>>> Welcome to my great game!
true.
-->

```logtalk
my_game(fr)::banner.
```

<!--
>>> Bienvenue sur mon grand jeu!
true.
-->

```logtalk
my_game(pt)::banner.
```

<!--
>>> Bem vindo ao meu grande jogo!
true.
-->
