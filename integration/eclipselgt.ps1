#############################################################################
## 
##   Integration script for ECLiPSe
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
		# One possibility is using HOME environment
		if (-not ($null -eq $env:HOME)) {
			$DEFAULTPATHS += $env:HOME + '\logtalk' #TODO really correct for windows?
		}
		
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

function Get-Logtalkuser {
	if ($null -eq $env:LOGTALKUSER) {
		Write-Output "After the script completion, you must set the environment variable"
		Write-Output "LOGTALKUSER pointing to %USERPROFILE%\Documents\Logtalk."
		env:LOGTALKUSER = "%USERPROFILE%\Documents\Logtalk"
	}
	# At the end LOGTALKUSER was set already or now is set
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

Get-Logtalkuser

# Check for existence
if (Test-Path $env:LOGTALKUSER) {
	if (!(Test-Path $env:LOGTALKUSER/VERSION.txt)) {
		Write-Output "Cannot find version information in the Logtalk user directory at %LOGTALKUSER%!"
		Write-Output "Creating an up-to-date Logtalk user directory..."
		logtalk_user_setup
	} else {
		$system_version = Get-Content $env:LOGTALKHOME/VERSION.txt
		$user_version = Get-Content $env:LOGTALKUSER/VERSION.txt
		if ($user_version -lt $system_version) {
			Write-Output "Logtalk user directory at %LOGTALKUSER% is outdated: "
			Write-Output "    $user_version < $system_version"
			Write-Output "Creating an up-to-date Logtalk user directory..."
			logtalk_user_setup
		}
	}
} else {
	Write-Output "Cannot find %LOGTALKUSER% directory! Creating a new Logtalk user directory"
	Write-Output "by running the logtalk_user_setup shell script:"
	logtalk_user_setup
}

$env:LOGTALK_STARTUP_DIRECTORY= $pwd

$source = $env:LOGTALKHOME + '\integration\logtalk_eclipse.pl'

if ($args.Count -gt 2 -and $args[$args.Count-2] -eq "--%") {
    $n = $args.Count - 3
    eclipse.exe -L iso -t user -f $source $args[0..$n] -- $args[$args.Count-1]
} elseif ($args.Count -eq 2 -and $args[0] -eq "--%") {
    eclipse.exe -L iso -t user -f $source -- $args[$args.Count-1]
} else {
    eclipse.exe -L iso -t user -f $source $args
}
