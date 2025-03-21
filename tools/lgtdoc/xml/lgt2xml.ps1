#############################################################################
## 
##   XML documenting files to XML conversion script 
##   Last updated on March 21, 2025
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


#Requires -Version 7.3

[CmdletBinding()]
param(
	[Parameter()]
	[String]$f = "xhtml",
	[String]$i = "index.html",
	[String]$t = "Documentation index",
	[Switch]$v,
	[Switch]$h
)

function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output "$myName 1.4"
}

function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output "This script generates an index for all the Logtalk XML files"
	Write-Output "documenting files in the current directory"
	Write-Output
	Write-Output "Usage:"
	Write-Output "  $myName [-f format] [-i index] [-t title]"
	Write-Output "  $myName -v"
	Write-Output "  $myName -h"
	Write-Output
	Write-Output "Optional arguments:"
	Write-Output "  -f output file format (either xhtml or html; default is $f)"
	Write-Output "  -i name of the index file (default is $i)"
	Write-Output "  -t title to be used in the index file (default is `"$t`")"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}

function Confirm-Parameters() {

	if ($f -ne "xhtml" -and $f -ne "html") {
		Write-Output "Error! Unknown output file format: $f"
		Start-Sleep -Seconds 2
		Exit 1
	}

	if ($v -eq $true) {
		Write-Script-Version
		Exit 0
	}

	if ($h -eq $true) {
		Write-Usage-Help
		Exit 0
	}

}

function New-Index-File() {
	New-Item -Path . -Name "$i" -ItemType "file" -Force > $null

	switch ( $f ) {
		"xhtml" {
			Add-Content -Path "$i" -Value "<?xml version=`"1.0`" encoding=`"utf-8`"?>"
			Add-Content -Path "$i" -Value "<?xml-stylesheet href=`"logtalk.css`" type=`"text/css`"?>"
			Add-Content -Path "$i" -Value "<!DOCTYPE html PUBLIC `"-//W3C//DTD XHTML 1.0 Strict//EN`" `"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd`">"
			Add-Content -Path "$i" -Value "<html lang=`"en`" xml:lang=`"en`" xmlns=`"http://www.w3.org/1999/xhtml`">"
		}
		"html" {
			Add-Content -Path "$i" -Value "<!DOCTYPE html PUBLIC `"-//W3C//DTD HTML 4.01//EN`" `"http://www.w3.org/TR/html4/strict.dtd`">"
			Add-Content -Path "$i" -Value "<html>"
		}
	}

	Add-Content -Path "$i" -Value "<head>"
	Add-Content -Path "$i" -Value "    <meta http-equiv=`"content-type`" content=`"text/html; charset=utf-8`"/>"
	Add-Content -Path "$i" -Value "    <title>$i</title>"
	Add-Content -Path "$i" -Value "    <link rel=`"stylesheet`" href=`"logtalk.css`" type=`"text/css`"/>"
	Add-Content -Path "$i" -Value "</head>"
	Add-Content -Path "$i" -Value "<body>"
	Add-Content -Path "$i" -Value "<h1>$i</h1>"
	Add-Content -Path "$i" -Value "<ul>"

	if (Test-Path "directory_index.xml") {
		Add-Content -Path "$i" -Value "    <li><a href=`"library_index.xml`">Library index</a></li>"
		Add-Content -Path "$i" -Value "    <li><a href=`"directory_index.xml`">Directory index</a></li>"
		Add-Content -Path "$i" -Value "    <li><a href=`"entity_index.xml`">Entity index</a></li>"
		Add-Content -Path "$i" -Value "    <li><a href=`"predicate_index.xml`">Predicate index</a></li>"
	} elseif (Get-ChildItem -Path . -Filter *.xml | Select-String -Pattern '<logtalk_entity' -CaseSensitive -SimpleMatch -Quiet) {
		Get-ChildItem -Path . -Filter *.xml |
		Foreach-Object {
			if (Select-String -Path $_.FullName -Pattern '<logtalk_entity' -CaseSensitive -SimpleMatch -Quiet) {
				$entity = ($_.BaseName -replace '_[^_]*$')
				$pars   = ($_.BaseName -replace '.*_')
				Write-Output "  indexing $($_.BaseName).html"
				if ([int]$pars -gt 0) {
					Add-Content -Path "$i" -Value "    <li><a href=`"$($_.Name)`">$entity/$pars</a></li>"
				} else {
					Add-Content -Path "$i" -Value "    <li><a href=`"$($_.Name)`">$entity</a></li>"
				}
			}
		}
	}

	Add-Content -Path "$i" -Value "</ul>"

	$date = Get-Date -Format "yyyy-MM-dd-HH:mm:ss"

	Add-Content -Path "$i" -Value "<p>Generated on $date</p>"
	Add-Content -Path "$i" -Value "</body>"
	Add-Content -Path "$i" -Value "</html>"
}

###################### here it starts ############################ 

Import-Module (Join-Path $PSScriptRoot "LogtalkSetup.psm1")
Initialize-LogtalkEnvironment

Confirm-Parameters

if (!(Test-Path "logtalk_entity.dtd")) {
	Copy-Item -Path "$env:LOGTALKHOME/tools/lgtdoc/xml/logtalk_entity.dtd" -Destination .
}

if (!(Test-Path "logtalk_index.dtd")) {
	Copy-Item -Path "$env:LOGTALKHOME/tools/lgtdoc/xml/logtalk_index.dtd" -Destination .
}

if (!(Test-Path "custom.ent")) {
	Copy-Item -Path "$env:LOGTALKUSER/tools/lgtdoc/xml/custom.ent" -Destination .
}

if (!(Test-Path "logtalk_entity.xsd")) {
	Copy-Item -Path "$env:LOGTALKHOME/tools/lgtdoc/xml/logtalk_entity.xsd" -Destination .
}

if (!(Test-Path "logtalk_index.xsd")) {
	Copy-Item -Path "$env:LOGTALKHOME/tools/lgtdoc/xml/logtalk_index.xsd" -Destination .
}

if (!(Test-Path "logtalk.css")) {
	Copy-Item -Path "$env:LOGTALKUSER/tools/lgtdoc/xml/logtalk.css" -Destination .
}

if (!(Test-Path "logtalk_entity_to_xml.xsl")) {
	Copy-Item -Path "$env:LOGTALKUSER/tools/lgtdoc/xml/logtalk_entity_to_xml.xsl" -Destination .
}

if (!(Test-Path "logtalk_index_to_xml.xsl")) {
	Copy-Item -Path "$env:LOGTALKUSER/tools/lgtdoc/xml/logtalk_index_to_xml.xsl" -Destination .
}

if (Test-Path -Path "*.xml" -PathType Leaf) {
	$hasLogtalkFiles =
		Get-ChildItem -Path "*.xml" -File | 
		Select-Object -First 1 | 
		Get-Content -Raw | 
		Select-String -Pattern '<logtalk' -CaseSensitive -SimpleMatch -Quiet
	
	if ($hasLogtalkFiles) {
		Write-Output "Indexing XML files..."
		Write-Output ""
		Write-Output "generating $i file..."
		New-Index-File
		Write-Output "$i file generated"
		Write-Output ""
	} else {
		Write-Output ""
		Write-Output "No Logtalk XML files exist in the current directory!"
		Write-Output ""
	}
} else {
	Write-Output ""
	Write-Output "No XML files exist in the current directory!"
	Write-Output ""
}
