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


This document describes how to configure, run and deploy the Logtalk Plugin
for IntelliJ.

## Preliminary steps

- Accomplish the preliminary steps for plugin development as recommended at
the [IntelliJ Platform SDK DevGuide](http://www.jetbrains.org/intellij/sdk/docs/basics/getting_started/setting_up_environment.html).
Particularly, verify that the _IntelliJ Platform SDK_ is properly configured.

## Configuring the project

- From the IntelliJ `File` menu, select `New -> Project...`.

- In the panel of the left, select: `IntelliJ Platform Plugin`. In the panel
of the right verify that the Project SDK corresponds to the IntelliJ SDK.
Click `Next`.

- In the `Project name` text field write: `logtalk-idea`.  In the `Project
location` text field write: `<LOGTALK_DIR>/coding/idea`. Click `Finish`.

- In the `Project` view, right click on the `gen` folder and select
`Mark Directory as` -> `Generated Sources Root`.


## Running the plugin

The first time you run the plugin you may need to create a `Run Configuration`.
Follow these steps:

- Select the `logtalk-idea` module in the `Project` view.

- From the `Run` menu choose `Run Plugin`.


## Preparing the plugin for deployment.

- Once the new version is looking good, update the version number and change
notes at the `plugin.xml` deployment descriptor. It is located in the
`resources/META-INF` directory.

- From the `Build` menu, select `Prepare Plugin Module <plugin name> for
deployment`.  A deployable plugin jar file will be generated in the plugin
directory.


## Deploying the plugin.

### Deployment to the IntelliJ Plugin Repository

- Update the plugin JAR at the [IntelliJ plugin site](https://plugins.jetbrains.com/plugin/9425-logtalk).
You will need to login first.


### Local Deployment

Take the jar file and copy it to your local IntelliJ plugin repository
(typically at `~/.IntelliJIdeaXXX/config/plugins`). You may need to restart
IntelliJ to see the changes applied. Do not forget to verify in the `Settings`
dialog that the plugin is enabled.
