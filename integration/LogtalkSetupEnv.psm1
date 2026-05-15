#############################################################################
##
##   Common code for PowerShell integration scripts
##   Last updated on May 15, 2026
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


function Test-LogtalkVersionIsOlder {
	$user_version = Get-Content (Join-Path $env:LOGTALKUSER 'VERSION.txt')
	$system_version = Get-Content (Join-Path $env:LOGTALKHOME 'VERSION.txt')

	$user_major, $user_minor, $user_patch, $user_suffix = $user_version -split '[.-]', 4
	$system_major, $system_minor, $system_patch, $system_suffix = $system_version -split '[.-]', 4

	if ([int]$user_major -lt [int]$system_major) {
		return $true
	}

	if ([int]$user_major -gt [int]$system_major) {
		return $false
	}

	if ([int]$user_minor -lt [int]$system_minor) {
		return $true
	}

	if ([int]$user_minor -gt [int]$system_minor) {
		return $false
	}

	if ([int]$user_patch -lt [int]$system_patch) {
		return $true
	}

	if ([int]$user_patch -gt [int]$system_patch) {
		return $false
	}

	return ($user_suffix -lt $system_suffix)
}


function Get-Logtalkhome {
	if ($null -eq $env:LOGTALKHOME) {
		Write-Output "The environment variable LOGTALKHOME should be defined first,"
		Write-Output "pointing to your Logtalk installation directory!"
		Write-Output "Trying the default locations for the Logtalk installation..."

		$DEFAULTPATHS = [string[]](
			(Join-Path ${env:ProgramFiles(x86)} "Logtalk"),
			(Join-Path $env:ProgramFiles "Logtalk"),
			(Join-Path $env:LOCALAPPDATA "Logtalk")
		)

		# Checking all default paths
		foreach ($DEFAULTPATH in $DEFAULTPATHS) {
			Write-Output "Looking for: $DEFAULTPATH"
			if (Test-Path $DEFAULTPATH) {
				Write-Output "... using Logtalk installation found at $DEFAULTPATH"
				$env:LOGTALKHOME = $DEFAULTPATH
				break
			}
		}
	}
}

function Get-Logtalkuser {
	if ($null -eq $env:LOGTALKUSER) {
		Write-Output "After the script completion, you must set the environment variable"
		Write-Output "LOGTALKUSER pointing to your Documents\Logtalk directory."
		$DocumentsPath = [Environment]::GetFolderPath("MyDocuments")
		$env:LOGTALKUSER = Join-Path $DocumentsPath "Logtalk"
	}
}

function Initialize-LogtalkEnvironment {
	Get-Logtalkhome

	if (!(Test-Path $env:LOGTALKHOME)) {
		Write-Error "... unable to locate Logtalk installation directory!"
		Write-Error ""
		Start-Sleep -Seconds 2
		Exit 1
	}

	Get-Logtalkuser

	if (Test-Path $env:LOGTALKUSER) {
		$VersionFile = "VERSION.txt"
		if (!(Test-Path (Join-Path $env:LOGTALKUSER $VersionFile))) {
			Write-Output "Cannot find $VersionFile in the Logtalk user directory at $env:LOGTALKUSER!"
			Write-Output "Creating an up-to-date Logtalk user directory..."
			logtalk_user_setup
		} else {
			if (Test-LogtalkVersionIsOlder) {
				$system_version = Get-Content (Join-Path $env:LOGTALKHOME $VersionFile)
				$user_version   = Get-Content (Join-Path $env:LOGTALKUSER $VersionFile)
				Write-Output "Logtalk user directory at $env:LOGTALKUSER is outdated: "
				Write-Output "    $user_version < $system_version"
				Write-Output "Creating an up-to-date Logtalk user directory..."
				logtalk_user_setup
			}
		}
	} else {
		Write-Output "Cannot find the Logtalk user directory at $env:LOGTALKUSER!"
		Write-Output "Running the logtalk_user_setup shell script to create the directory:"
		logtalk_user_setup
	}

	$env:LOGTALK_STARTUP_DIRECTORY = (Get-Location).Path
}

Export-ModuleMember -Function Initialize-LogtalkEnvironment
