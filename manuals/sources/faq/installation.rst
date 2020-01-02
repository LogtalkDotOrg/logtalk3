..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _faq_installation:

Installation
============

* :ref:`faq_installation_scripts`
* :ref:`faq_installation_startup_errors`


.. _faq_installation_scripts:

The integration scripts/shortcuts are not working!
--------------------------------------------------

Check that the ``LOGTALKHOME`` and ``LOGTALKUSER`` environment
variables are defined, that the Logtalk user folder is available on
the location pointed by ``LOGTALKUSER`` (you can create this folder
by running the ``logtalk_user_setup`` shell script), and that the Prolog
compilers that you want to use are supported and available from the
system path. If the problem persists, run the shell script that
creates the integration script or shortcut manually and check for any
error message or additional instructions. For some Prolog compilers
such as XSB and Ciao, the first call of the integration script or
shortcut must be made by an administrator user. If you are using
Windows, make sure that any anti-virus or other security software
that you might have installed is not silently blocking some of the
installer tasks.

.. _faq_installation_startup_errors:

I get errors when starting up Logtalk after upgrading to the latest version!
----------------------------------------------------------------------------

Changes in the Logtalk compiler between releases may render Prolog
adapter files from older versions incompatible with new ones. You may
need to update your local Logtalk user files by running e.g. the
``logtalk_user_setup`` shell script. Check the ``UPGRADING.md`` file
on the root of the Logtalk installation directory and the release
notes for any incompatible changes to the adapter files.
