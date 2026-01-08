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
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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

# recipes

This example is inspired by a StackOverflow question for which a sketch of a
Logtalk-based solution was provided:

	http://stackoverflow.com/questions/26119110/knowledge-representation-in-prolog-how-to-store-data

It illustrates a possible solution for representing structured data using
objects and also hot patching of running code. It also shows when a mixed
representation using both data objects and Prolog facts can be managed.

The main files defined in this example are:

- `specs.lgt`  
	defines the recipe protocol, `recipep`, declaring recipe description
	predicates and a prototype object, `proto_recipe`, defining handy
	predicates for summarizing recipe information
- `recipes.lgt`  
	defines some recipes represented as objects
- `parametric.lgt`  
	defines some recipes represented as facts for a `recipe/3` predicate
	plus a `recipe/3` parametric object and a `recipe/1` predicate for
	enumerating recipes while abstracting their representation

- `patch_1.lgt` and `patch_2.lgt`  
	categories used to illustrate hot patching of the example objects

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(recipes(loader)).
```

List all the ingredients required for the green soup recipe:

```logtalk
green_soup::ingredients(Ingredients).
```

<!--
Ingredients = [peas,cream,water,oil].
-->

List all the steps required to make mashed peas:

```logtalk
mashed_peas::steps(Steps).
```

<!--
Steps = [1-'Boil the peas.',2-'Mash the peas',3-'Add salt and mix.'].
-->

Find out what dishes you can cook with green peas (we check that
we have a ground recipe to skip the generic recipe represented 
by the `recipe/3` parametric object):

```logtalk
%%table
conforms_to_protocol(Recipe, recipep), ground(Recipe), Recipe::ingredient(peas).
```

<!--
Recipe = green_soup ? ;
Recipe = mashed_peas ? ;
false.
-->

Patch the prototypical recipe to add a `level/1` descriptor to all recipes:

```logtalk
logtalk_load(recipes(patch_1)).
```

<!--
true.
-->

Recipes are now easy by default:

```logtalk
mashed_peas::level(Level).
```

<!--
Level = easy.
-->

But you always get into trouble while making green soup; add a second patch
to define the level value for this recipe to be "hard":

```logtalk
logtalk_load(recipes(patch_2)).
```

<!--
true.
-->

Confirm the patched level:

```logtalk
green_soup::level(Level).
```

<!--
Level = hard.
-->

When using a mixed representation, the `conforms_to_protocol/2` built-in
predicate is not ideal to enumerate recipes as it will also return the
parametric object used for accessing recipes represented as Prolog facts:

```logtalk
%%table
conforms_to_protocol(Recipe, recipep).
```

<!--
Recipe = green_soup ;
Recipe = mashed_peas ;
Recipe = recipe(_, _, _) ;
false.
-->

Thus, to enumerate recipes, is best to use the `recipe/1` predicate, which
ensures that we always get a proper recipe:

```logtalk
%%table
recipe(Recipe).
```

<!--
Recipe = green_soup ;
Recipe = mashed_peas ;
Recipe = recipe('Chocolate Chip Cookies', [flour-500-gr, butter-20-gr, chocolate-200-gr, sugar-65-gr, eggs-2-units], ['mix flour, butter and sugar'-10-min, 'add eggs and mix'-10-min, 'make chocolate chips'-5-min, 'split in small portions'-5-min, 'cook in the oven'-25-min]) ;
Recipe = recipe('Berries and cream', [cream-500-ml, sugar-50-gr, strawberries-300-gr, chocolate-100-gr], ['mix whipping cream add sugar'-5-min, 'slice strawberries'-5-min, 'place alternate layers of cream and strawberries in dessert dishes'-10-min, 'make chocolate chips'-5-min, 'top dishes with chocolate chips'-5-min]) ;
false.
-->

An usage example of the `recipe/1` predicate would be:

```logtalk
%%table
recipe(Recipe), Recipe::cooking_time(CookingTime).
```

<!--
Recipe = green_soup, CookingTime = 36 ;
Recipe = mashed_peas, CookingTime = 16 ;
Recipe = recipe('Chocolate Chip Cookies', [flour-500-gr, butter-20-gr, chocolate-200-gr, sugar-65-gr, eggs-2-units], ['mix flour, butter and sugar'-10-min, 'add eggs and mix'-10-min, 'make chocolate chips'-5-min, 'split in small portions'-5-min, 'cook in the oven'-25-min]), CookingTime = 55 ;
Recipe = recipe('Berries and cream', [cream-500-ml, sugar-50-gr, strawberries-300-gr, chocolate-100-gr], ['mix whipping cream add sugar'-5-min, 'slice strawberries'-5-min, 'place alternate layers of cream and strawberries in dessert dishes'-10-min, 'make chocolate chips'-5-min, 'top dishes with chocolate chips'-5-min]), CookingTime = 30 ;
false.
-->
