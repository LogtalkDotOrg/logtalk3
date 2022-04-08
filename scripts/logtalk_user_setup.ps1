#############################################################################
## 
##   Logtalk user folder setup script
##   Last updated on April 5, 2022
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 2022 Hans N. Beck and Paulo Moura <pmoura@logtalk.org>
##   SPDX-License-Identifier: Apache-2.0
##   
##   Licensed under the Apache License, Version 2.0 (the "License");
##   you may not use this file except in compliance with the License.
##   You may obtain a copy of the License at
##   
##       http://www.apache.org/licenses/LICENSE-2.0
##   
##   Unless required by applicable law or agreed to in writing, software
##   distributed under the License is distributed on an "AS IS" BASIS,
##   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
##   See the License for the specific language governing permissions and
##   limitations under the License.
## 
#############################################################################

[CmdletBinding()]
param ()

function Describe-Script {
	Write-Output ""
	Write-Output "This script copies all the Logtalk per-user files and directories to the"
	Write-Output "user home directory. The location can be set by the environment variable"
	Write-Output "%LOGTALKUSER% (default value is %USERPROFILE%\Documents\Logtalk)."
	Write-Output ""
}

Describe-Script

function Get-Logtalkhome {
	if ($null -eq $env:LOGTALKHOME) 
	{
		Write-Output "The environment variable LOGTALKHOME should be defined first, pointing"
		Write-Output "to your Logtalk installation directory!"
		Write-Output "Trying the default locations for the Logtalk installation..."
		
		$DEFAULTPATHS = [string[]](
			"C:\Program Files (x86)\Logtalk",
			"C:\Program Files\Logtalk",
			"%LOCALAPPDATA%\Logtalk"
		)
		
		# Checking all possibilites
		foreach ($DEFAULTPATH in $DEFAULTPATHS) { 
			Write-Output ("Looking for: " + $DEFAULTPATH)
			if (Test-Path $DEFAULTPATH) {
				Write-Output ("... using Logtalk installation found at " + $DEFAULTPATH)
				$env:LOGTALKHOME = $DEFAULTPATH
				break
			}
		}
	}
	# At the end LOGTALKHOME was set already or now is set
}

Get-Logtalkhome

# Check for existence
if (Test-Path $env:LOGTALKHOME) {
	Write-Output ("Using Logtalk installation found at: " + $env:LOGTALKHOME)
	Write-Output ""
} else {
	Write-Output "... unable to locate Logtalk installation directory!"
	Write-Output ""
	Start-Sleep -Seconds 2
	Exit
}

function Get-Logtalkuser {
	if ($null -eq $env:LOGTALKUSER) {
		Write-Output "After the script completion, you must set the environment variable"
		Write-Output "LOGTALKUSER pointing to %USERPROFILE%\Documents\Logtalk."
		$env:LOGTALKUSER = "%USERPROFILE%\Documents\Logtalk"
	}
	# At the end LOGTALKUSER was set already or now is set
}

Get-Logtalkuser

function Create-Logtalkuser-Directory {
	if (Test-Path $env:LOGTALKUSER) {
		$date = Get-Date -Format "yyyy-MM-dd-HHmmss"
		Move-Item -Path $env:LOGTALKUSER -Destination $env:LOGTALKUSER-$date
		Write-Output "Created a backup of the existing %LOGTALKUSER% directory:"
		Write-Output ""
		Write-Output ("    " + "$env:LOGTALKUSER-$date")
		Write-Output ""
		Write-Output "Creating a new %LOGTALKUSER% directory:"
		Write-Output ""
		Write-Output ("    " + $env:LOGTALKUSER)
		Write-Output ""
		New-Item -Path $env:LOGTALKUSER -ItemType directory
		if (Test-Path $env:LOGTALKUSER-$date\settings.lgt) {
			Copy-Item $env:LOGTALKUSER-$date\settings.lgt -Destination $env:LOGTALKUSER
			Write-Output "Copied your old \"settings.lgt\" file to the new \"\%LOGTALKUSER\%\" directory."
		}
		if (Test-Path $env:LOGTALKUSER-$date\settings.logtalk) {
			Copy-Item $env:LOGTALKUSER-$date\settings.logtalk -Destination $env:LOGTALKUSER
			Write-Output "Copied your old \"settings.logtalk\" file to the new \"\%LOGTALKUSER\%\" directory."
		}
		if (Test-Path $env:LOGTALKUSER-$date\packs) {
			New-Item -Path $env:LOGTALKUSER\packs -ItemType directory
			Copy-Item -Path $env:LOGTALKUSER-$date\packs -Destination $env:LOGTALKUSER\packs -Recurse
			Write-Output "Copied your packs to the new \"\%LOGTALKUSER\%\" directory."
		}
	} else {
		Write-Output "Creating a new %LOGTALKUSER% directory:"
		Write-Output ("    " + $env:LOGTALKUSER)
		New-Item -Path $env:LOGTALKUSER -ItemType directory
	}

	Write-Output "Copying Logtalk files and directories..."
	Copy-Item -Path $env:LOGTALKHOME\contributions -Destination $env:LOGTALKUSER\contributions -Recurse
	Copy-Item -Path $env:LOGTALKHOME\docs -Destination $env:LOGTALKUSER\docs -Recurse
	Copy-Item -Path $env:LOGTALKHOME\examples -Destination $env:LOGTALKUSER\examples -Recurse
	Copy-Item -Path $env:LOGTALKHOME\library -Destination $env:LOGTALKUSER\library -Recurse
	Copy-Item -Path $env:LOGTALKHOME\ports -Destination $env:LOGTALKUSER\ports -Recurse
	Copy-Item -Path $env:LOGTALKHOME\scratch -Destination $env:LOGTALKUSER\sratch -Recurse
	Copy-Item -Path $env:LOGTALKHOME\tests -Destination $env:LOGTALKUSER\tests -Recurse
	Copy-Item -Path $env:LOGTALKHOME\tools -Destination $env:LOGTALKUSER\tools -Recurse
	Copy-Item -Path $env:LOGTALKHOME\loader-sample.lgt -Destination $env:LOGTALKUSER\loader-sample.lgt
	Copy-Item -Path $env:LOGTALKHOME\settings-sample.lgt -Destination $env:LOGTALKUSER\settings-sample.lgt
	Copy-Item -Path $env:LOGTALKHOME\tester-sample.lgt -Destination $env:LOGTALKUSER\tester-sample.lgt
	Copy-Item -Path $env:LOGTALKHOME\tests-sample.lgt -Destination $env:LOGTALKUSER\tests-sample.lgt
	Copy-Item -Path $env:LOGTALKHOME\VERSION.txt -Destination $env:LOGTALKUSER\VERSION.txt
	Remove-Item -Path $env:LOGTALKUSER\tools\diagrams\lgt2*.*
	Remove-Item -Path $env:LOGTALKUSER\tools\lgtdoc\xml\lgt2*.*
	Remove-Item -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_entity.dtd
	Remove-Item -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_entity.rng
	Remove-Item -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_entity.xsd
	Remove-Item -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_index.dtd
	Remove-Item -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_index.rng
	Remove-Item -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_index.xsd
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\ACKNOWLEDGMENTS.md -Target $env:LOGTALKHOME\ACKNOWLEDGMENTS.md
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\BIBLIOGRAPHY.bib -Target $env:LOGTALKHOME\BIBLIOGRAPHY.bib
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\CONTRIBUTING.md -Target $env:LOGTALKHOME\CONTRIBUTING.md
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\CUSTOMIZE.md -Target $env:LOGTALKHOME\CUSTOMIZE.md
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\INSTALL.md -Target $env:LOGTALKHOME\INSTALL.md
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\LICENSE.txt -Target $env:LOGTALKHOME\LICENSE.txt
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\QUICK_START.md -Target $env:LOGTALKHOME\QUICK_START.md
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\README.md -Target $env:LOGTALKHOME\README.md
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\RELEASE_NOTES.md -Target $env:LOGTALKHOME\RELEASE_NOTES.md
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\UPGRADING.md -Target $env:LOGTALKHOME\UPGRADING.md
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\adapters -Target $env:LOGTALKHOME\adapters
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\coding -Target $env:LOGTALKHOME\coding
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\integration -Target $env:LOGTALKHOME\integration
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\manuals -Target $env:LOGTALKHOME\manuals
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\paths -Target $env:LOGTALKHOME\paths
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\scripts -Target $env:LOGTALKHOME\scripts
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\diagrams\lgt2svg.bat -Target $env:LOGTALKHOME\tools\diagrams\lgt2svg.bat
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\lgt2html.bat -Target $env:LOGTALKHOME\tools\lgtdoc\xml\lgt2html.bat
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\lgt2pdf.bat -Target $env:LOGTALKHOME\tools\lgtdoc\xml\lgt2pdf.bat
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\lgt2xml.bat -Target $env:LOGTALKHOME\tools\lgtdoc\xml\lgt2xml.bat
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\lgt2md.bat -Target $env:LOGTALKHOME\tools\lgtdoc\xml\lgt2md.bat
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\lgt2txt.bat -Target $env:LOGTALKHOME\tools\lgtdoc\xml\lgt2txt.bat
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_entity.dtd -Target $env:LOGTALKHOME\tools\lgtdoc\xml\logtalk_entity.dtd
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_entity.rng -Target $env:LOGTALKHOME\tools\lgtdoc\xml\logtalk_entity.rng
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_entity.xsd -Target $env:LOGTALKHOME\tools\lgtdoc\xml\logtalk_entity.xsd
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_index.dtd -Target $env:LOGTALKHOME\tools\lgtdoc\xml\logtalk_index.dtd
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_index.rng -Target $env:LOGTALKHOME\tools\lgtdoc\xml\logtalk_index.rng
	New-Item -ItemType SymbolicLink -Path $env:LOGTALKUSER\tools\lgtdoc\xml\logtalk_index.xsd -Target $env:LOGTALKHOME\tools\lgtdoc\xml\logtalk_index.xsd

	Write-Output "Finished copying Logtalk files and directories."
	Write-Output ""
	Write-Output "You may want to customize the default Logtalk compiler flags by renaming"
	Write-Output "and editing the settings-sample.lgt source file found in the directory"
	Write-Output "%LOGTALKUSER%. For more information on how to customize Logtalk and the"
	Write-Output "working environment, consult the %LOGTALKUSER%\CUSTOMIZE.md file."
	Write-Output ""
}

Create-Logtalkuser-Directory
