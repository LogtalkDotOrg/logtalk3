#############################################################################
## 
##   XML documenting files to PDF conversion script 
##   Last updated on April 12, 2022
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
param(
	[Parameter()]
	[String]$f = "a4", 
	[String]$d = $pwd, 
	[String]$p = "fop", 
	[Switch]$v,
	[Switch]$h
)

function Get-ScriptVersion {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output ($myName + " 2.0")
}

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

function Get-Logtalkuser {
	if ($null -eq $env:LOGTALKUSER) {
		Write-Output "After the script completion, you must set the environment variable"
		Write-Output "LOGTALKUSER pointing to %USERPROFILE%\Documents\Logtalk."
		$env:LOGTALKUSER = "%USERPROFILE%\Documents\Logtalk"
	}
	# At the end LOGTALKUSER was set already or now is set
}

function Get-Usage() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output "This script converts all Logtalk XML documenting files in the"
	Write-Output "current directory to PDF files"
	Write-Output ""
	Write-Output "Usage:"
	Write-Output ("  " + $myName + " [-f format] [-d directory] [-p processor]")
	Write-Output ("  " + $myName + " -v")
	Write-Output ("  " + $myName + " -h")
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output ("  -f output file format (either a4 or us; default is " + $f + ")")
	Write-Output ("  -d output directory for the generated files (default is " + $d + ")")
	Write-Output ("  -p XSL-FO processor (either fop, fop2, xep, or xinc; default is " + $d + ")")
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}

function Check-Parameters() {

	if ($f -ne "a4" -and $f -ne "us") {
		Write-Output ("Error! Unsupported output format: " + $f)
		Start-Sleep -Seconds 2
		Exit
	}

	if (-not(Test-Path $d)) { # cannot be ""
		Write-Output ("The " + $p + " output directory does not exist!")
		Start-Sleep -Seconds 2
		Exit
	}

	if ($p -ne "fop" -and $p -ne "fop2" -and $p -ne "xep" -and $p -ne "xinc") {
		Write-Output ("Error! Unsupported XSL-FO processor: " + $f)
		Start-Sleep -Seconds 2
		Exit
	}

	if ($v -eq $true) {
		Get-ScriptVersion
		Exit
	}

	if ($h -eq $true) {
		Get-Usage
		Exit
	}

}

###################### here it starts ############################ 

Check-Parameters

Get-Logtalkhome

# Check for existence
if (Test-Path $env:LOGTALKHOME) {
	$output = "Found LOGTALKHOME at: " + $env:LOGTALKHOME
	Write-Output $output
} else {
	Write-Output "... unable to locate Logtalk installation directory!"
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

if ($f -eq "a4") {
	$entity_xslt =$env:LOGTALKUSER + "\tools\lgtdoc\xml\logtalk_entity_to_pdf_a4.xsl"
} else {
	$entity_xslt  =$env:LOGTALKUSER + "\tools\lgtdoc\xml\logtalk_entity_to_pdf_us.xsl"
}

if (!(Test-Path "logtalk_entity.dtd")) {
	Copy-Item -Path $env:LOGTALKHOME\tools\lgtdoc\xml\logtalk_entity.dtd -Destination .
}

if (!(Test-Path "custom.ent")) {
	Copy-Item -Path $env:LOGTALKUSER\tools\lgtdoc\xml\custom.ent -Destination .
}

if (!(Test-Path "logtalk_entity.xsd")) {
	Copy-Item -Path $env:LOGTALKHOME\tools\lgtdoc\xml\logtalk_entity.xsd -Destination .
}


if (Select-String -Path .\*.xml -Pattern '<logtalk' -CaseSensitive -SimpleMatch -Quiet) {
	Write-Output "Converting XML files to PDF files..."

	Get-ChildItem -Path .\*.xml |
	Foreach-Object {
		if (Select-String -Path $_ -Pattern '<logtalk_entity' -CaseSensitive -SimpleMatch -Quiet) {
			Write-Output ("  converting " + $_.Name)
			$file = Join-Path $pwd $_.Name
			$pdf = Join-Path $d ($_.BaseName + ".pdf")
			if ( $p -eq  "xinc") {
				 xinc -xml "$file" -xsl "$entity_xslt" -pdf "$pdf" > $null
			} else {
				 & $p -q -xml "$file" -xsl "$entity_xslt" -pdf "$pdf" > $null			
			}
		}
	}
	Write-Output "conversion done"
	Write-Output ""
} else {
	Write-Output ""
	Write-Output "No XML files exist in the current directory!"
	Write-Output ""
}

if ($pwd -ne (Join-Path $env:LOGTALKHOME xml)) {
	Remove-Item -Path .\logtalk_entity.dtd
	Remove-Item -Path .\logtalk_entity.xsd
	Remove-Item -Path .\custom.ent
}
