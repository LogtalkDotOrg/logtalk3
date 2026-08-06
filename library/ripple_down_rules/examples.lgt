%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(tennis).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-06,
		comment is 'Executable examples for the Ripple-Down Rules library.'
	]).

	:- public(tennis_models/2).
	:- mode(tennis_models(-compound, -compound), one).
	:- info(tennis_models/2, [
		comment is 'Returns the SCRDR models before and after correcting the classic tennis/squash storm case.',
		argnames is ['Before', 'After']
	]).

	:- public(tennis_cornerstone/1).
	:- mode(tennis_cornerstone(-list(pair)), one).
	:- info(tennis_cornerstone/1, [
		comment is 'Returns the local companion cornerstone used by the root tennis rule.',
		argnames is ['Case']
	]).

	:- public(squash_storm_case/1).
	:- mode(squash_storm_case(-list(pair)), one).
	:- info(squash_storm_case/1, [
		comment is 'Returns the classic storm case that is initially misclassified as tennis and corrected to squash.',
		argnames is ['Case']
	]).

	:- public(mcrdr_models/3).
	:- mode(mcrdr_models(-compound, -compound, -compound), one).
	:- info(mcrdr_models/3, [
		comment is 'Returns MCRDR models before correction, after selectively removing tennis, and after adding squash.',
		argnames is ['Before', 'Stopped', 'After']
	]).

	:- public(grdr_model/1).
	:- mode(grdr_model(-compound), one).
	:- info(grdr_model/1, [
		comment is 'Returns a GRDR model whose equipment conclusion depends on a sport conclusion inferred in an earlier pass.',
		argnames is ['Model']
	]).

	:- public(sunny_cool/2).
	:- mode(sunny_cool(+list(pair), +list(pair)), zero_or_one).
	:- info(sunny_cool/2, [
		comment is 'True when a case has sunny outlook and cool temperature.',
		argnames is ['Case', 'Conclusions']
	]).

	:- public(windy_humid/2).
	:- mode(windy_humid(+list(pair), +list(pair)), zero_or_one).
	:- info(windy_humid/2, [
		comment is 'True when a case has windy wind and high humidity.',
		argnames is ['Case', 'Conclusions']
	]).

	:- public(windy/2).
	:- mode(windy(+list(pair), +list(pair)), zero_or_one).
	:- info(windy/2, [
		comment is 'True when a case has windy wind.',
		argnames is ['Case', 'Conclusions']
	]).

	:- public(tennis/3).
	:- mode(tennis(@term, +list(pair), --atom), one).
	:- info(tennis/3, [
		comment is 'Returns the tennis conclusion.',
		argnames is ['Case', 'Conclusions', 'Conclusion']
	]).

	:- public(squash/3).
	:- mode(squash(@term, +list(pair), --atom), one).
	:- info(squash/3, [
		comment is 'Returns the squash conclusion.',
		argnames is ['Case', 'Conclusions', 'Conclusion']
	]).

	:- public(kite_flying/3).
	:- mode(kite_flying(@term, +list(pair), --atom), one).
	:- info(kite_flying/3, [
		comment is 'Returns the kite_flying conclusion.',
		argnames is ['Case', 'Conclusions', 'Conclusion']
	]).

	:- public(sport_squash/2).
	:- mode(sport_squash(+list(pair), +list(pair)), zero_or_one).
	:- info(sport_squash/2, [
		comment is 'True when squash is available as either a case feature or a keyed sport conclusion.',
		argnames is ['Case', 'Conclusions']
	]).

	:- public(racket/3).
	:- mode(racket(@term, +list(pair), --atom), one).
	:- info(racket/3, [
		comment is 'Returns the racket conclusion.',
		argnames is ['Case', 'Conclusions', 'Conclusion']
	]).

	:- uses([
		single_classification_ripple_down_rules as scrdr,
		multi_classification_ripple_down_rules as mcrdr,
		generalized_ripple_down_rules as grdr
	]).

	:- uses(list, [
		memberchk/2
	]).

	tennis_models(Before, After) :-
		tennis_cornerstone(Cornerstone),
		squash_storm_case(Storm),
		scrdr::new(Empty, [default(none)]),
		scrdr::revise(Empty, Cornerstone, replace, tennis::sunny_cool, tennis::tennis, Before),
		scrdr::revise(Before, Storm, replace, tennis::windy_humid, tennis::squash, After).

	mcrdr_models(Before, Stopped, After) :-
		tennis_cornerstone(Cornerstone),
		squash_storm_case(Storm),
		mcrdr::new(Empty),
		mcrdr::revise(Empty, Cornerstone, add, tennis::sunny_cool, tennis::tennis, TennisModel),
		mcrdr::revise(TennisModel, Storm, add, tennis::windy, tennis::kite_flying, Before),
		mcrdr::revise(Before, Storm, remove, tennis::windy_humid, tennis::tennis, Stopped),
		mcrdr::revise(Stopped, Storm, add, tennis::windy_humid, tennis::squash, After).

	grdr_model(Model) :-
		squash_storm_case(Storm),
		EquipmentCase = [sport-squash],
		mcrdr::new(EmptyEquipment),
		mcrdr::revise(EmptyEquipment, EquipmentCase, add, tennis::sport_squash, tennis::racket, Equipment),
		scrdr::new(EmptySport, [default(none)]),
		scrdr::revise(EmptySport, Storm, replace, tennis::windy_humid, tennis::squash, Sport),
		grdr::new(Empty),
		grdr::put(Empty, equipment, Equipment, WithEquipment),
		grdr::put(WithEquipment, sport, Sport, Model).

	tennis_cornerstone([
		outlook-sunny,
		temperature-cool,
		wind-calm,
		humidity-normal,
		forecast-clear
	]).

	squash_storm_case([
		outlook-sunny,
		temperature-cool,
		wind-windy,
		humidity-high,
		forecast-storm
	]).

	sunny_cool(Case, _) :-
		memberchk(outlook-sunny, Case),
		memberchk(temperature-cool, Case).

	windy_humid(Case, _) :-
		memberchk(wind-windy, Case),
		memberchk(humidity-high, Case).

	windy(Case, _) :-
		memberchk(wind-windy, Case).

	sport_squash(Case, _) :-
		memberchk(sport-squash, Case),
		!.
	sport_squash(_, Conclusions) :-
		memberchk(sport-squash, Conclusions).

	tennis(_, _, tennis).

	squash(_, _, squash).

	kite_flying(_, _, kite_flying).

	racket(_, _, racket).

:- end_object.


% Compact, independently expressed adaptation of the relational robot
% containment scenario exercised by the Python Ripple-Down Rules project.

:- object(robot_containment).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-06,
		comment is 'Executable relational SCRDR example for robot-part containment.'
	]).

	:- public(model/1).
	:- mode(model(-compound), one).
	:- info(model/1, [
		comment is 'Returns a relational SCRDR containment model.',
		argnames is ['Model']
	]).

	:- public(case/2).
	:- mode(case(?atom, ?list(pair)), zero_or_more).
	:- info(case/2, [
		comment is 'Enumerates example robot-part cases.',
		argnames is ['Name', 'Case']
	]).

	:- public(has_container/2).
	:- mode(has_container(+list(pair), +list(pair)), zero_or_one).
	:- info(has_container/2, [
		comment is 'True when a part has a container.',
		argnames is ['Case', 'Conclusions']
	]).

	:- public(contained/3).
	:- mode(contained(+list(pair), +list(pair), --compound), one).
	:- info(contained/3, [
		comment is 'Returns the immediate containment relation.',
		argnames is ['Case', 'Conclusions', 'Conclusion']
	]).

	:- uses([
		single_classification_ripple_down_rules as scrdr
	]).

	:- uses(list, [
		memberchk/2
	]).

	case(wheel, [part-wheel,container-chassis,robot-rover]).
	case(camera, [part-camera,container-mast,robot-rover]).
	case(loose_bolt, [part-bolt]).

	model(Model) :-
		case(wheel, Wheel),
		scrdr::new(Empty, [default(uncontained)]),
		scrdr::revise(Empty, Wheel, replace, robot_containment::has_container, robot_containment::contained, Model).

	has_container(Case, _) :-
		memberchk(container-_, Case).

	contained(Case, _, contained(Part, Container)) :-
		memberchk(part-Part, Case), memberchk(container-Container, Case).

:- end_object.


% Compact, independently expressed adaptation of the drawer/cabinet view
% recognition scenario exercised by the Python Ripple-Down Rules project.

:- object(furniture_recognition).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-06,
		comment is 'Executable MCRDR example for simultaneous drawer and cabinet recognition.'
	]).

	:- public(model/1).
	:- mode(model(-compound), one).
	:- info(model/1, [
		comment is 'Returns a multi-label furniture recognition model.',
		argnames is ['Model']
	]).

	:- public(scene/1).
	:- mode(scene(-list(pair)), one).
	:- info(scene/1, [
		comment is 'Returns a scene containing drawer and cabinet evidence.',
		argnames is ['Case']
	]).

	:- public(has_drawer/2).
	:- mode(has_drawer(+list(pair), +list(pair)), zero_or_one).
	:- info(has_drawer/2, [
		comment is 'True when drawer evidence is present.',
		argnames is ['Case', 'Conclusions']
	]).

	:- public(has_cabinet/2).
	:- mode(has_cabinet(+list(pair), +list(pair)), zero_or_one).
	:- info(has_cabinet/2, [
		comment is 'True when cabinet evidence is present.',
		argnames is ['Case', 'Conclusions']
	]).

	:- public(drawer/3).
	:- mode(drawer(@term, +list(pair), --atom), one).
	:- info(drawer/3, [
		comment is 'Returns the drawer label.',
		argnames is ['Case', 'Conclusions', 'Conclusion']
	]).

	:- public(cabinet/3).
	:- mode(cabinet(@term, +list(pair), --atom), one).
	:- info(cabinet/3, [
		comment is 'Returns the cabinet label.',
		argnames is ['Case', 'Conclusions', 'Conclusion']
	]).

	:- uses([
		multi_classification_ripple_down_rules as mcrdr
	]).

	:- uses(list, [
		memberchk/2
	]).

	scene([horizontal_handle-1,enclosed_volume-1,vertical_door-1]).

	model(Model) :-
		scene(Case),
		mcrdr::new(Empty),
		mcrdr::revise(Empty, Case, add, furniture_recognition::has_drawer, furniture_recognition::drawer, DrawerModel),
		mcrdr::revise(DrawerModel, Case, add, furniture_recognition::has_cabinet, furniture_recognition::cabinet, Model).

	has_drawer(Case, _) :-
		memberchk(horizontal_handle-1, Case).

	has_cabinet(Case, _) :-
		memberchk(enclosed_volume-1, Case), memberchk(vertical_door-1, Case).

	drawer(_, _, drawer).

	cabinet(_, _, cabinet).

:- end_object.


% Compact, independently expressed adaptation of the molecular mutagenicity
% scenario exercised by the Python Ripple-Down Rules project.

:- object(mutagenicity).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-06,
		comment is 'Executable SCRDR example for structural molecular mutagenicity classification.'
	]).

	:- public(model/1).
	:- mode(model(-compound), one).
	:- info(model/1, [
		comment is 'Returns a structural mutagenicity model.',
		argnames is ['Model']
	]).

	:- public(molecule/2).
	:- mode(molecule(?atom, ?list(pair)), zero_or_more).
	:- info(molecule/2, [
		comment is 'Enumerates compact molecule feature cases.',
		argnames is ['Name', 'Case']
	]).

	:- public(nitro_aromatic/2).
	:- mode(nitro_aromatic(+list(pair), +list(pair)), zero_or_one).
	:- info(nitro_aromatic/2, [
		comment is 'True for an aromatic molecule containing a nitro group.',
		argnames is ['Case', 'Conclusions']
	]).

	:- public(mutagenic/3).
	:- mode(mutagenic(@term, +list(pair), --atom), one).
	:- info(mutagenic/3, [
		comment is 'Returns the mutagenic conclusion.',
		argnames is ['Case', 'Conclusions', 'Conclusion']
	]).

	:- uses([
		single_classification_ripple_down_rules as scrdr
	]).

	:- uses(list, [
		memberchk/2
	]).

	molecule(nitrobenzene, [aromatic-1,nitro_group-1,halogen-0]).
	molecule(ethanol, [aromatic-0,nitro_group-0,halogen-0]).

	model(Model) :-
		molecule(nitrobenzene, Case),
		scrdr::new(Empty, [default(non_mutagenic)]),
		scrdr::revise(Empty, Case, replace, mutagenicity::nitro_aromatic, mutagenicity::mutagenic, Model).

	nitro_aromatic(Case, _) :-
		memberchk(aromatic-1, Case), memberchk(nitro_group-1, Case).

	mutagenic(_, _, mutagenic).

:- end_object.


% The seven rows below are unchanged representatives of classes 1 through 7
% from Richard Forsyth's Zoo dataset (1990), DOI 10.24432/C5R59V, licensed
% under CC BY 4.0 by the UCI Machine Learning Repository.

:- object(zoo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-06,
		comment is 'Executable SCRDR and GRDR examples using seven representative rows from the UCI Zoo dataset.'
	]).

	:- public(representative/3).
	:- mode(representative(?atom, ?list(pair), ?integer), zero_or_more).
	:- info(representative/3, [
		comment is 'Enumerates one unchanged representative feature vector and numeric label for each Zoo class.',
		argnames is ['Animal', 'Features', 'Class']
	]).

	:- public(scrdr_model/1).
	:- mode(scrdr_model(-compound), one).
	:- info(scrdr_model/1, [
		comment is 'Returns an SCRDR model classifying the seven representative Zoo rows.',
		argnames is ['Model']
	]).

	:- public(grdr_model/1).
	:- mode(grdr_model(-compound), one).
	:- info(grdr_model/1, [
		comment is 'Returns a GRDR model inferring species and habitat for the seven representative Zoo rows.',
		argnames is ['Model']
	]).

	:- public(species_condition/3).
	:- mode(species_condition(+integer, +list(pair), +list(pair)), zero_or_one).
	:- info(species_condition/3, [
		comment is 'Recognizes the feature pattern used for a Zoo class.',
		argnames is ['Class', 'Case', 'Conclusions']
	]).

	:- public(species_conclusion/4).
	:- mode(species_conclusion(+atom, @term, +list(pair), --atom), one).
	:- info(species_conclusion/4, [
		comment is 'Returns a species conclusion.',
		argnames is ['Species', 'Case', 'Conclusions', 'Conclusion']
	]).

	:- public(species_is/3).
	:- mode(species_is(+atom, +list(pair), +list(pair)), zero_or_one).
	:- info(species_is/3, [
		comment is 'True when a species conclusion occurs in the case or keyed conclusions.',
		argnames is ['Species', 'Case', 'Conclusions']
	]).

	:- public(habitat_conclusion/4).
	:- mode(habitat_conclusion(+atom, @term, +list(pair), --atom), one).
	:- info(habitat_conclusion/4, [
		comment is 'Returns a habitat conclusion.',
		argnames is ['Habitat', 'Case', 'Conclusions', 'Conclusion']
	]).

	:- uses([
		single_classification_ripple_down_rules as scrdr,
		multi_classification_ripple_down_rules as mcrdr,
		generalized_ripple_down_rules as grdr
	]).

	:- uses(list, [
		memberchk/2
	]).

	representative(aardvark, [hair-1,feathers-0,eggs-0,milk-1,airborne-0,aquatic-0,predator-1,toothed-1,backbone-1,breathes-1,venomous-0,fins-0,legs-4,tail-0,domestic-0,catsize-1], 1).
	representative(chicken,  [hair-0,feathers-1,eggs-1,milk-0,airborne-1,aquatic-0,predator-0,toothed-0,backbone-1,breathes-1,venomous-0,fins-0,legs-2,tail-1,domestic-1,catsize-0], 2).
	representative(pitviper, [hair-0,feathers-0,eggs-1,milk-0,airborne-0,aquatic-0,predator-1,toothed-1,backbone-1,breathes-1,venomous-1,fins-0,legs-0,tail-1,domestic-0,catsize-0], 3).
	representative(bass,     [hair-0,feathers-0,eggs-1,milk-0,airborne-0,aquatic-1,predator-1,toothed-1,backbone-1,breathes-0,venomous-0,fins-1,legs-0,tail-1,domestic-0,catsize-0], 4).
	representative(frog,     [hair-0,feathers-0,eggs-1,milk-0,airborne-0,aquatic-1,predator-1,toothed-1,backbone-1,breathes-1,venomous-0,fins-0,legs-4,tail-0,domestic-0,catsize-0], 5).
	representative(flea,     [hair-0,feathers-0,eggs-1,milk-0,airborne-0,aquatic-0,predator-0,toothed-0,backbone-0,breathes-1,venomous-0,fins-0,legs-6,tail-0,domestic-0,catsize-0], 6).
	representative(clam,     [hair-0,feathers-0,eggs-1,milk-0,airborne-0,aquatic-0,predator-1,toothed-0,backbone-0,breathes-0,venomous-0,fins-0,legs-0,tail-0,domestic-0,catsize-0], 7).

	scrdr_model(Model) :-
		representative(aardvark, MammalCase, 1),
		representative(chicken, BirdCase, 2),
		representative(pitviper, ReptileCase, 3),
		representative(bass, FishCase, 4),
		representative(frog, AmphibianCase, 5),
		representative(flea, InsectCase, 6),
		representative(clam, InvertebrateCase, 7),
		scrdr::new(Empty),
		add_species(Empty, MammalCase, 1, mammal, MammalModel),
		add_species(MammalModel, BirdCase, 2, bird, BirdModel),
		add_species(BirdModel, ReptileCase, 3, reptile, ReptileModel),
		add_species(ReptileModel, FishCase, 4, fish, FishModel),
		add_species(FishModel, AmphibianCase, 5, amphibian, AmphibianModel),
		add_species(AmphibianModel, InsectCase, 6, insect, InsectModel),
		add_species(InsectModel, InvertebrateCase, 7, invertebrate, Model).

	grdr_model(Model) :-
		scrdr_model(Species),
		habitat_model(Habitat),
		grdr::new(Empty),
		grdr::put(Empty, habitat, Habitat, WithHabitat),
		grdr::put(WithHabitat, species, Species, Model).

	add_species(Model, Case, Class, Species, NewModel) :-
		scrdr::revise(Model, Case, replace, zoo::species_condition(Class), zoo::species_conclusion(Species), NewModel).

	habitat_model(Model) :-
		mcrdr::new(Empty),
		add_habitat(Empty, mammal, land, MammalModel),
		add_habitat(MammalModel, bird, aerial, BirdModel),
		add_habitat(BirdModel, reptile, land, ReptileModel),
		add_habitat(ReptileModel, fish, aquatic, FishModel),
		add_habitat(FishModel, amphibian, wetland, AmphibianModel),
		add_habitat(AmphibianModel, insect, land, InsectModel),
		add_habitat(InsectModel, invertebrate, aquatic, Model).

	add_habitat(Model, Species, Habitat, NewModel) :-
		Case = [species-Species],
		mcrdr::revise(Model, Case, add, zoo::species_is(Species), zoo::habitat_conclusion(Habitat), NewModel).

	species_condition(1, Case, _) :-
		memberchk(milk-1, Case).
	species_condition(2, Case, _) :-
		memberchk(feathers-1, Case).
	species_condition(3, Case, _) :-
		memberchk(venomous-1, Case), memberchk(backbone-1, Case).
	species_condition(4, Case, _) :-
		memberchk(fins-1, Case).
	species_condition(5, Case, _) :-
		memberchk(aquatic-1, Case), memberchk(legs-4, Case).
	species_condition(6, Case, _) :-
		memberchk(legs-6, Case).
	species_condition(7, Case, _) :-
		memberchk(backbone-0, Case), memberchk(legs-0, Case).

	species_conclusion(Species, _, _, Species).

	species_is(Species, Case, _) :-
		memberchk(species-Species, Case),
		!.
	species_is(Species, _, Conclusions) :-
		memberchk(species-Species, Conclusions).

	habitat_conclusion(Habitat, _, _, Habitat).

:- end_object.


:- object(grdr_cycle).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-06,
		comment is 'Executable GRDR example with mutually dependent conclusions that do not converge.'
	]).

	:- public(model/1).
	:- mode(model(-compound), one).
	:- info(model/1, [
		comment is 'Returns a GRDR model configured to report non-convergence after three cycles.',
		argnames is ['RDR']
	]).

	:- public(key_value/3).
	:- mode(key_value(+pair, @term, +list(pair)), zero_or_one).
	:- info(key_value/3, [
		comment is 'True when a keyed value is visible to a rule.',
		argnames is ['Pair', 'Case', 'Conclusions']
	]).

	:- public(on/3).
	:- mode(on(@term, +list(pair), --atom), one).
	:- info(on/3, [
		comment is 'Returns the on conclusion.',
		argnames is ['Case', 'Conclusions', 'Conclusion']
	]).

	:- uses([
		single_classification_ripple_down_rules as scrdr,
		generalized_ripple_down_rules as grdr
	]).

	:- uses(list, [
		memberchk/2
	]).

	model(Model) :-
		scrdr::new(EmptyA, [default(off)]),
		scrdr::revise(EmptyA, [b-on], replace, grdr_cycle::key_value(b-on), grdr_cycle::on, A),
		scrdr::new(EmptyB, [default(off)]),
		scrdr::revise(EmptyB, [a-off], replace, grdr_cycle::key_value(a-off), grdr_cycle::on, B),
		grdr::new(Empty, [maximum_cycles(3)]),
		grdr::put(Empty, a, A, WithA),
		grdr::put(WithA, b, B, Model).

	key_value(Pair, Case, _) :-
		memberchk(Pair, Case),
		!.
	key_value(Pair, _, Conclusions) :-
		memberchk(Pair, Conclusions).

	on(_, _, on).

:- end_object.
