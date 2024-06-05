________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 2017 Sergio Castro <sergioc78@gmail.com>  
SPDX-FileCopyrightText: 2017 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains files and resources required to run Logtalk in a
Docker container. It also contains the readme files that are displayed at
Docker Hub. Each one of these directories includes the following files:

- `Dockerfile`  
	the docker file configuring Logtalk with a specific Prolog backend
	or a selection of the supported backends

- `README.md`  
	the `README.md` file automatically used as the project description
	when creating an automated build at Docker Hub

The Docker images are available for downloading at the Docker Hub website:

	https://hub.docker.com/u/logtalk/
