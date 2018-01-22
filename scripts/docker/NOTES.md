________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 2017 Sergio Castro <sergioc78@gmail.com> and  
Paulo Moura <pmoura@logtalk.org>

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
Docker container. It also contains the read-me files that are displayed
at Docker Hub. The files are organized in directories per supported
backend Prolog compiler. Note that we are currently limited to Prolog
systems with available Docker containers. Each one of these directories
includes the following files:

- `Dockerfile`  
	the docker file configuring Logtalk with a specific Prolog engine

- `README.md`  
	the `README.md` file automatically used as the project description
	when creating an automated build at Docker Hub

The stable and nightly Docker images are available for downloading at
the Docker Hub website:

	https://hub.docker.com/u/logtalk/
