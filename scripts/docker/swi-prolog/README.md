________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
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


Logtalk Docker configuration for SWI-Prolog
===========================================

Build arguments
---------------

- `LOGTALK_VERSION`  
The Logtalk version to be built.

Valid identifiers are as shown in the Logtalk GitHub repository at [release names](https://github.com/LogtalkDotOrg/logtalk3/releases). Defaults to `master`, which causes the Docker image to be built with the latest version of Logtalk in its master branch.

Volumes
-------

- `/source`  
The work directory where Logtalk is started. The code base can be mounted here.

- `/logtalkuser_prefix`  
If the user would like to customize its installation, he could mount in this volume the parent directory in its host system where it is located the custom Logtalk user directory. If no Logtalk user directory exists, it will be created the first time and will be persisted and used in the following executions of the Logtalk container.


Building a Logtalk image
------------------------

#### Nightly build

	docker build -t="logtalk/logtalk3-swi:nightly" .

#### Last stable release

	docker build --build-arg LOGTALK_VERSION=lgt3091stable -t="logtalk/logtalk3-swi:3091" -t="logtalk/logtalk3-swi:latest" .


Running a Logtalk container
---------------------------

#### From the nightly build

	docker run -it --name logtalk_nightly "logtalk/logtalk3-swi:nightly"


#### From the last stable release

	docker run -it --name logtalk_latest "logtalk/logtalk3-swi:latest"
