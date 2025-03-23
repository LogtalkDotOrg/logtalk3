#############################################################################
##
##   XML documenting files to plain text conversion script
##   Last updated on March 23, 2025
##
##   This file is part of Logtalk <https://logtalk.org/>
##   Copyright 2022-2025 Paulo Moura <pmoura@logtalk.org>
##   Copyright 2022 Hans N. Beck
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
	[String]$d = $pwd,
	[Switch]$v,
	[Switch]$h
)

function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output "$myName 2.4"
}

function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve

	Write-Output "This script converts all Logtalk XML documenting files in the"
	Write-Output "current directory to plain text files"
	Write-Output ""
	Write-Output "Usage:"
	Write-Output "  $myName [-d directory]"
	Write-Output "  $myName -v"
	Write-Output "  $myName -h"
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -d output directory for the generated files (default is $d)"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}

function Confirm-Parameters() {

	if (-not(Test-Path $d)) { # cannot be ""
		Write-Error "The $d output directory does not exist!"
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

###################### here it starts ############################

Import-Module (Join-Path $PSScriptRoot "LogtalkSetupEnv.psm1")
Initialize-LogtalkEnvironment

Confirm-Parameters

$entity_xslt = "$env:LOGTALKUSER/tools/lgtdoc/xml/logtalk_entity_to_txt.xsl"
$index_xslt = "$env:LOGTALKUSER/tools/lgtdoc/xml/logtalk_index_to_txt.xsl"

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


if (Select-String -Path .\*.xml -Pattern '<logtalk' -CaseSensitive -SimpleMatch -Quiet) {
	Write-Output "Converting XML files to plain text files..."

	$xslt_settings = New-Object System.Xml.Xsl.XsltSettings
	$xslt_settings.EnableDocumentFunction = $true

	$xml_reader_settings = New-Object System.Xml.XmlReaderSettings
	$xml_reader_settings.DtdProcessing = [System.Xml.DtdProcessing]::Ignore

	$xml_url_resolver = New-Object System.Xml.XmlUrlResolver

	$entity_xslt_object = New-Object System.Xml.Xsl.XslCompiledTransform;
	$entity_xslt_object.Load($entity_xslt, $xslt_settings, $xml_url_resolver)

	$index_xslt_object = New-Object System.Xml.Xsl.XslCompiledTransform;
	$index_xslt_object.Load($index_xslt, $xslt_settings, $xml_url_resolver)

	Get-ChildItem -Path . -Filter *.xml |
	Foreach-Object {
		if (Select-String -Path $_ -Pattern '<logtalk_entity' -CaseSensitive -SimpleMatch -Quiet) {
			Write-Output "  converting $($_.Name)"
			$file = Join-Path "$pwd" "$($_.Name)"
			$text = Join-Path $d "$($_.BaseName).txt"
			$reader = [System.Xml.XmlReader]::Create($file, $xml_reader_settings)
			$fs = New-Object IO.FileStream $text, 'Append', 'Write', 'Read'
			$writer = New-Object System.IO.StreamWriter($fs)
			$entity_xslt_object.Transform($reader, $null, $writer)
			$writer.Close()
			$reader.Dispose()
		}
	}
	Get-ChildItem -Path . -Filter *.xml |
	Foreach-Object {
		if (Select-String -Path $_ -Pattern '<logtalk_index' -CaseSensitive -SimpleMatch -Quiet) {
			Write-Output "  converting $($_.Name)"
			$file = Join-Path "$pwd" "$($_.Name)"
			$text = Join-Path "$d" "$($_.BaseName).txt"
			$reader = [System.Xml.XmlReader]::Create($file, $xml_reader_settings)
			$fs = New-Object IO.FileStream $text, 'Append', 'Write', 'Read'
			$writer = New-Object System.IO.StreamWriter($fs)
			$index_xslt_object.Transform($reader, $null, $writer)
			$writer.Close()
			$reader.Dispose()
		}
	}
	Write-Output "conversion done"
	Write-Output ""
} else {
	Write-Output ""
	Write-Output "No XML files exist in the current directory!"
	Write-Output ""
}

if ($pwd -ne (Join-Path "$env:LOGTALKHOME" xml)) {
	Remove-Item -Path .\logtalk_entity.dtd
	Remove-Item -Path .\logtalk_entity.xsd
	Remove-Item -Path .\logtalk_index.dtd
	Remove-Item -Path .\logtalk_index.xsd
	Remove-Item -Path .\custom.ent
}
