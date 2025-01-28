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

# puzzles

This folder contains examples of Logtalk implementations for popular 
logical puzzles. The description of each puzzle can be found on the 
source files themselves.

Start by loading the example and the required library files:

```logtalk
logtalk_load(puzzles(loader)).
```

Harry Potter's room of potions logical puzzle:

```logtalk
potions::potions(P1, P2, P3, P4, P5, P6, P7).
```

<!--
P1 = poison, P2 = wine, P3 = forward, P4 = poison, P5 = poison, P6 = wine,  P7 = backwards.
-->

Horse show logical puzzle:

```logtalk
horses::(horses(S), print(S)).
```

<!--
1 place: april riding doc, the chestnut gelding
2 place: sue riding danny, the bay gelding
3 place: doc riding gopher, the gray gelding
4 place: danny riding april, the white mare
5 place: gopher riding sue, the black mare

S = [h(doc,gelding,chestnut,april,1),h(danny,gelding,bay,sue,2),h(gopher,gelding,gray,doc,3),h(april,mare,white,danny,4),h(sue,mare,black,gopher,5)].
-->

Who Stole the Jam?

```logtalk
jam_thief::thief(Thief).
```

<!--
Thief = hare.
-->

```logtalk
jam_thief::thief(Thief, Why).
```

<!--
Thief = hare, Why = [trusty(dormouse),liar(hare),trusty(hatter)].
-->

Houses logical puzzle:

```logtalk
houses::(houses(Solution), print(Solution)).
```

<!--
h(norwegian,fox,kool,water,yellow)
h(ukrainian,horse,chesterfield,tea,blue)
h(english,snake,winston,milk,red)
h(spanish,dog,lucky,juice,white)
h(japonese,zebra,kent,coffee,green)
Solution = [h(norwegian, fox, kool, water, yellow), h(ukrainian, horse, chesterfield, tea, blue), h(english, snake, winston, milk, red), h(spanish, dog, lucky, juice, white), h(japonese, zebra, kent, coffee, green)] ;
h(norwegian,fox,kool,water,yellow)
h(ukrainian,horse,chesterfield,tea,blue)
h(english,snake,winston,milk,red)
h(japonese,zebra,kent,coffee,green)
h(spanish,dog,lucky,juice,white)
Solution = [h(norwegian, fox, kool, water, yellow), h(ukrainian, horse, chesterfield, tea, blue), h(english, snake, winston, milk, red), h(japonese, zebra, kent, coffee, green), h(spanish, dog, lucky, juice, white)] ;
false.
-->

```logtalk
houses::zebra_owner(Owner).
```

<!--
Owner = japonese ;
Owner = japonese ;
false.
-->

```logtalk
houses::water_drinker(Drinker).
```

<!--
Drinker = norwegian ;
Drinker = norwegian ;
false.
-->

Passing a note logical puzzle:

```logtalk
note::(students(S), print(S)).
```

<!--
s(mary,english,red,1)
s(paul,math,yellow,2)
s(josephine,science,green,3)
s(derrick,french,blue,4)
s(alexis,reading,black,5)

S = [s(mary,english,red,1),s(paul,math,yellow,2),s(josephine,science,green,3),s(derrick,french,blue,4),s(alexis,reading,black,5)].
-->

Mort's Letter from Camp Swampy logical puzzle:

```logtalk
camp_swampy::(beds(S), print(S)).
```

<!--
tim thomas, from maine, sleeps on bed number 1
sam franklin, from north_carolina, sleeps on bed number 2
mac miller, from virginia, sleeps on bed number 3
fred james, from florida, sleeps on bed number 4
john smith, from arkansas, sleeps on bed number 5

S = [b(tim,thomas,maine,1),b(sam,franklin,north_carolina,2),b(mac,miller,virginia,3),b(fred,james,florida,4),b(john,smith,arkansas,5)].
-->
