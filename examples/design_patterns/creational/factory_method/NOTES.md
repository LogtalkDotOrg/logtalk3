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

Design pattern:
	Factory Method

Description:
	"Define an interface for creating an object, but let subclasses
	decide which class to instantiate. Factory Method lets a class
	defer instantiation to subclasses."

This pattern can be used with both classes and prototypes.

The sample implementation uses prototypes for simplicity with categories
playing a similar role to abstract classes.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('creational/factory_method/loader')).
```

Create an ordinary maze game:

```logtalk
ordinary_maze_game::new(Game), Game::play.
```

<!--
Playing using:
o2 - Ordinary room
o3 - Ordinary room
Game = o1

true.
-->

Create a magic maze game:

```logtalk
magic_maze_game::new(Game), Game::play.
```

<!--
Playing using:
o5 - Magic room
o6 - Magic room
Game = o4

true.
-->
