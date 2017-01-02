%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% some recipes (warning: cooking is really not one of my skills...)


:- object(green_soup,
	implements(recipep),
	extends(proto_recipe)).

    name('Green Soup').

    ingredient(peas, 500, gr).
    ingredient(cream, 200, ml).
    ingredient(water, 1000, ml).
    ingredient(oil, 10, ml).

    step(1, 'Boil the peas.', 10).
    step(2, 'Mash the peas', 5).
    step(3, 'Add salt and mix.', 1).
    step(4, 'Add mashed peas to water and stir.', 10).
    step(5, 'Add oil and stir.', 10).

:- end_object.


:- object(mashed_peas,
	implements(recipep),
	extends(proto_recipe)).

    name('Mashed Peas').

    ingredient(peas, 700, gr).
    ingredient(salt, 20, gr).

    step(1, 'Boil the peas.', 10).
    step(2, 'Mash the peas', 5).
    step(3, 'Add salt and mix.', 1).

:- end_object.
