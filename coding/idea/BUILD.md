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

- Install the IntelliJ [Gradle plugin](https://www.jetbrains.com/help/idea/gradle.html).

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



## Running the plugin

This plugin uses Gradle. In order to build and test the plugin from IntelliJ, open the Gradle view:
(`View` menu, select `Tool Windows -> Gradle`). 
In the Gradle view, available Gradle tasks can be found under `Tasks -> intellij`.

To see the plugin in action, execute the `runIde` task. 
A new IntelliJ instance will run with the plugin installed.



## Preparing the plugin for deployment.

- Once the new version is looking good, update the version number and change
notes at the `plugin.xml` deployment descriptor. It is located in the
`resources/META-INF/` directory.

- In order to build the plugin, execute the `buildPlugin` Gradle task. 
A deployable plugin jar file will be generated under the `build/distributions/` directory.


## Deploying the plugin.

### Deployment to the IntelliJ Plugin Repository

- Update the plugin JAR at the [IntelliJ plugin site](https://plugins.jetbrains.com/plugin/9425-logtalk).
You will need to login first.


### Local Deployment

Take the jar file and copy it to your local IntelliJ plugin repository
(typically at `~/.IntelliJIdeaXXX/config/plugins`). You may need to restart
IntelliJ to see the changes applied. Do not forget to verify in the `Settings`
dialog that the plugin is enabled.
