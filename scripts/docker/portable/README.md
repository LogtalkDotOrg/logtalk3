________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 2024-2025 Paulo Moura <pmoura@logtalk.org>  
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


# Logtalk Docker image

Includes Logtalk and a subset of the supported (and experimental/legacy)
Prolog backends:

- B-Prolog (legacy)
- Ciao Prolog (experimental)
- CxProlog
- ECLiPSe
- GNU Prolog
- SWI-Prolog
- Tau Prolog
- Trealla Prolog
- XSB
- YAP

Includes Jupyter, JupyterLab, and the Logtalk kernel.

Includes all Logtalk developer tool dependencies. 

Includes the `nano` text editor for small editing tasks. For actual
development, use the VSCode or VSCodium recommended setups as described
below.

Includes a SSH server (not running by default) configured with user `root`
and password `portable`. Can be started using the `service ssh start`
command.


## Build arguments and their defaults

* `LOGTALK_VERSION` (`master`)
* `LOGTALK_TEXINFO_VERSION` (3.92.0)

Valid identifiers are as shown in the Logtalk GitHub repository at
[release names](https://github.com/LogtalkDotOrg/logtalk3/releases).

* `B_VERSION` (`81`)
* `CIAO_VERSION` (`v1.23.0-m1`)
* `CX_VERSION` (`0.98.3`)
* `ECLIPSE_VERSION` (`7.1_13`)
* `GNU_VERSION` (`master`)
* `SWI_VERSION` (`swipl-devel` repo; `master`)
* `TAU_VERSION` (`0.3.4`)
* `TREALLA_VERSION` (`main`)
* `XSB_VERSION` (`git-origin`)
* `YAP_VERSION` (`master`)

Valid identifiers are as shown in the backend repositories or download
websites. Setting the backend version build argument to `none` skips
installing the backend.


## Building a local image

With all the backends supported by the Docker file:

	docker build -t logtalk3 .

Excluding a backend from the image. For example, excluding the B-Prolog legacy
system:

	docker build -t logtalk3 --build-arg B_VERSION=none .


## Volumes

* `/source`  
The working directory where Logtalk is started. The source of your project can
be mounted here.


## Running a container

### Providing a shell:

	docker run -it --name test logtalk3

You can then run Logtalk with any of the installed backends. For details, see:

https://github.com/LogtalkDotOrg/logtalk3/blob/master/QUICK_START.md

### Starting the SSH server on a running container:

	docker run -it --name test -p 127.0.0.1:2222:22 logtalk3
	docker exec -it test service ssh restart

You can then connect to the server using the command:

	 ssh root@localhost -p 2222

Enter the password `portable` when prompted.

### Adding a `~/project` directory as a volume:

	docker run -it --name test -v /home/jdoe/project:/source logtalk3

Note that you need to use absolute paths for your project directory.


## Editing your project


### Using VSCode

1. Install the "Dev Containers" extension provided by Microsoft (note that
this extension is not available for VSCodium; see below for an alternative
solution).

2. Create a new container while mounting your project directory as explained
above.

3. In the VSCode lower left corner, click on the "Open a Remote Window" icon
and select the option "Attach to Running Container...". Select the container
you're running.

4. Install the "Logtalk Extension Pack" extension in the container. This pack
includes the "Logtalk for VSCode" extension. See below how to configure it.

5. Open the project directory mounted in the container under `/source` by using
the "File" menu "Open Folder..." item.

In alternative, open your local project using the "File" menu "Open Folder..."
item and add dev container configuration files to it:

1. Create a `.devcontainer/devcontainer.json` file at the root of your local
project and add the following contents to it (using the SWI-Prolog backend as
an example; update the setting if using another backend):

	```json
	{
	    "name": "Logtalk",
	    "image": "logtalk/logtalk3-portable:latest",
	    "customizations": {
	        "vscode": {
	            "extensions": [
	                "logtalkdotorg.logtalk-extension-pack"
	            ],
	            "settings": {
	                "logtalk.home.path": "/usr/local/share/logtalk",
	                "logtalk.user.path": "/root/logtalk",
	                "logtalk.backend": "swi"
	            }
	         }
	    },
	    "forwardPorts": [8989]
	}
	```

The forwarded port can be used to browse Allure test reports from the host by
running in a container terminal the command:

	allure open -p 8989

Change the port number if it conflicts with any service running on your host.

2. In the VSCode lower left corner, click on the "Open a Remote Window" icon
and select the option "Reopen in Container".

3. Install the "Logtalk Extension Pack" extension in the remote. This pack
includes the "Logtalk for VSCode" extension. See below how to configure it.


### Using VSCodium

1. Install the "Open Remote - SSH" extension provided by `jeanp413`.

2. Create a new container while mounting your project directory and starting
the SSH server as explained above. Forward an additional port using a second
`-p` option (e.g. `-p 127.0.0.1:8998:8998`) if you want to browse Allure test
reports from the host by running in a container terminal the command:

	allure open -p 8989

Change the port number if it conflicts with any service running on your host.

3. In the VSCodium lower left corner, click on the "Open a Remote Window" icon
and select the option "Connect to Host...". Type `root@localhost:2222`. Enter
the password `portable` when prompted.

4. Install the "Logtalk Extension Pack" extension in the SSH remote. This pack
includes the "Logtalk for VSCode" extension. See below how to configure it.

5. Open the project directory mounted in the container under `/source` by using
the "File" menu "Open Folder..." item.


## Configuring the "Logtalk for VSCode" extension

The "Logtalk for VSCode" extension can be configured by opening the "Settings"
pane, typing "Logtalk" in the search box, and selecting "Remote" settings. The
required settings are:

- "logtalk.home.path" - `/usr/local/share/logtalk`
- "logtalk.user.path" - `/root/logtalk`
- "logtalk.backend"

The backend identifier must be for one of the installed Prolog systems:

- B-Prolog: `b`
- Ciao Prolog: `ciao`
- CxProlog: `cx`
- ECLiPSe: `eclipse`
- GNU Prolog: `gnu`
- SWI-Prolog: `swi`
- Tau Prolog: `tau`
- Trealla Prolog: `trealla`
- XSB: `xsb`
- YAP: `yap`


## Creating Jupyter notebooks

Install the "Jupyter" extension in the container if missing (it's included
in the "Logtalk Extension Pack" extension). Create a new notebook by
selecting the "File" menu "New File..." item and selecting the Jupyter
notebook file type or open an existing notebook in your mounted volume.
If necessary, select the Logtalk kernel by clicking in the "Select Kernel"
button in the top right corner of the notebook file.

To select the backend used to run the notebook (default is SWI-Prolog),
create in the same directory a file named `logtalk_kernel_config.py` and
edit it to select the backend. A copy of this file can be found in the
image in the `/root/.jupyter` directory where the default backend is set.

A Logtalk kernel overview notebook and a Logtalk tutorial notebook can be
found at:

https://github.com/LogtalkDotOrg/logtalk-jupyter-kernel/tree/master/notebooks


## Running the JupyterLab server

Open a new integrated terminal in VSCode and run the following command from
the container directory containing your notebooks:

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


## Running the examples documentation as Jupyter notebooks

Most of the examples `NOTES.md` files can be open as Jupyter notebooks.
Open a new integrated terminal in VSCode and run the following commands:

	cd /root/logtalk/examples  
	jupyter-lab --allow-root --NotebookApp.token='' --NotebookApp.password=''

You can then open the examples `NOTES.md` files as notebooks in JupyterLab by
control-clicking on them and selecting the "Open With" > "Notebook" option.

To configure the backend to be used when opening the examples `NOTES.md`
files as notebooks (default is SWI-Prolog), edit the image file
`/root/.jupyter/logtalk_kernel_config.py`. Currently, only ECLiPSe,
SWI-Prolog, and Trealla Prolog can reliably be used as backends in the
notebooks.
