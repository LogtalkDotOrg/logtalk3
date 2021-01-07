%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Adapter design pattern:
%
% https://en.wikipedia.org/wiki/Adapter_pattern


% we start by defining a protocol that specifies the recharge process

:- protocol(recharge).

	:- public(recharge/0).

:- end_protocol.


% as the recharge process happens through interfaces that depend on the
% type of the phone, iPhone or Android in our example, we extend the
% basic recharge protocol to account for the interface specifics

:- protocol(iphone_recharge,
	extends(recharge)).

	:- public(use_lightning/0).

:- end_protocol.


:- protocol(android_recharge,
	extends(recharge)).

	:- public(use_micro_usb/0).

:- end_protocol.


% we can now define the two phone types in our example, whose
% functionality includes recharging

:- object(iphone,
	implements(iphone_recharge)).

	recharge :-
		write('Recharge Started'), nl,
		write('  Recharge 25%'), nl,
		write('  Recharge 50%'), nl,
		write('  Recharge 75%'), nl,
		write('Recharge Finished'), nl.

	use_lightning :-
		write('Lightning connected'), nl.

:- end_object.


:- object(android,
	implements(android_recharge)).

	recharge :-
		write('Recharge Started'), nl,
		write('  Recharge 20%'), nl,
		write('  Recharge 40%'), nl,
		write('  Recharge 60%'), nl,
		write('  Recharge 80%'), nl,
		write('Recharge Finished'), nl.

	use_micro_usb :-
		write('MicroUsb connected'), nl.

:- end_object.


% we now define an adapter that allows using a generic USB recharger
% with an iPhone (which features a Lightning interface); the adapter uses
% the Android phone recharge interface with a specific implementation
% (representing a Lightning <-> Micro USB connector) that allows generic
% rechargers, playing here the role of the clients, to be used to
% recharge an iPhone

:- object(iphone_adapter(_IPHONE_),
	implements(android_recharge)).

	recharge :-
		_IPHONE_::recharge.

	use_micro_usb :-
		write('MicroUsb connected'), nl,
		_IPHONE_::use_lightning.

:- end_object.


% the clients are rechargers; we define a simple protocol
% for the action of connecting a recharger with a phone

:- protocol(recharger).

	:- public(connect/0).

:- end_protocol.


% we can now define our rechargers by implementing the protocol;
% the rechargers use the phones interfaces, possibly by means of
% an adapter

:- object(iphone_recharger(_PHONE_),
	implements(recharger)).

	connect :-
		_PHONE_::use_lightning,
		_PHONE_::recharge.

:- end_object.


:- object(android_recharger(_PHONE_),
	implements(recharger)).

	connect :-
		_PHONE_::use_micro_usb,
		_PHONE_::recharge.

:- end_object.


:- object(iphone_micro_usb_recharger(_PHONE_),
	implements(recharger)).

	connect :-
		iphone_adapter(_PHONE_)::use_micro_usb,
		iphone_adapter(_PHONE_)::recharge.

:- end_object.
