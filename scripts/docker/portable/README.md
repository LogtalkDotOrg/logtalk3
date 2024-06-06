________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 2024 Paulo Moura <pmoura@logtalk.org>  
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


Logtalk Docker image
====================

Includes Logtalk and a subset of the supported Prolog backends:

- Ciao Prolog
- CxProlog
- ECLiPSe
- GNU Prolog
- SWI-Prolog
- Trealla Prolog
- XSB
- YAP

Also includes Jupyter, JupyterLab, and the Logtalk kernel.


Build arguments and their defaults
----------------------------------

* `LOGTALK_VERSION` (`master`)

Valid identifiers are as shown in the Logtalk GitHub repository at
[release names](https://github.com/LogtalkDotOrg/logtalk3/releases).

* `CIAO_VERSION` (`v1.23.0-m1`)
* `CX_VERSION` (`0.98.3`)
* `ECLIPSE_VERSION` (`7.1_13`)
* `GNU_VERSION` (`master`)
* `SWI_VERSION` (`master`)
* `TREALLA_VERSION` (`main`)
* `XSB_VERSION` (`git-origin`)
* `YAP_VERSION` (`master`)

Valid identifiers are as shown in the backend repositories or download
websites.


Volumes
-------

* `/source`  
The work directory where Logtalk is started. The source of your project can
be mounted here.


Building a local image
----------------------

	docker build -t logtalk3 .


Running a container
-------------------

#### Providing a shell

	docker run -it --name=test logtalk3

#### Adding a ~/project directory as a volume

	docker run -it --name test -v /home/jdoe/project:/source logtalk3


Editing your project
--------------------

The recommended solution to work in a project using the image resources is
to mount the project directory and then use VSCode with the "Dev Containers"
extension installed:

1. Run the Docker image while mounting your project directory as a volume
(see above; you must use absolute paths).

2. In the VSCode lower left corner, click on the "Open a Remote Window" icon.

3. Select the option "Attach to Running Container..." and select the container
you're running.

4. Open the project directory mounted in the container by using the "File" menu
"Open Folder..." item.

5. Install the "Logtalk for VSCode" extension in the container and configure
it by going into "Settings", typing "Logtalk" in the search box, and selecting
"Remote" settings.

Note that you can open an integrated terminal in the remote to find the values
to be used for the configuration of the Logtalk extension. For example:

	echo $LOGTALKHOME

For the value of the `LOGTALKUSER` environment variable, you'll need the full
path to `~/logtalk`. By default, the container user is `root` and therefore
the `LOGTALKUSER` environment variable would be set to `/root/logtalk`.

Similar for the integration script that you want to use. For example:

	which gplgt


Creating Jupyter notebooks
--------------------------

Install the "Jupyter" extension in the container. Create a new notebook by
selecting the "File" menu "New File..." item and selecting the Jupyter
notebook file type or open an existing notebook in your mounted volume.
If necessary, select the Logtalk kernel by clicking in the "Select Kernel"
button in the top right corner of the notebook file.

To select the backend used to run the notebook (default is SWI-Prolog),
create in the same directory a file named "logtalk_kernel_config.py" and
edit it to select the backend. A copy of this file can be found at:

	/usr/local/lib/python3.10/dist-packages/logtalk_kernel

Alternatively, download the file from:

https://github.com/LogtalkDotOrg/logtalk-jupyter-kernel/tree/master/logtalk_kernel


Running the JupyterLab server
-----------------------------

Open a new integrated terminal in VSCode and run the following command:

	jupyter-lab --allow-root --NotebookApp.token='' --NotebookApp.password=''

VSCode will display a dialog with a button to open the JupyterLab start page
on your host web browser. If you need a different port than the default 8888,
e.g. 8891, use instead:

	jupyter-lab --allow-root --NotebookApp.token='' --NotebookApp.password='' --port=8891

See the JupyterLab documentation for more details on available options.

To check that everything is running, create a new Logtalk notebook using the
JupyterLab interface and type in a cell:

	%versions

Executing the cell (by default, Shift-Enter) should print the Logtalk version,
the backend version, and the Logtalk Jupyter kernel version.
