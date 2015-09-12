<?xml version="1.0" encoding="utf-8"?>

<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>    
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-->

<xsl:stylesheet
	version="1.0"
	xmlns="http://www.w3.org/1999/xhtml"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xhtml="http://www.w3.org/1999/xhtml"
	exclude-result-prefixes="#default">

<xsl:output
	method="xml"
    indent="yes"
    encoding="utf-8"
	omit-xml-declaration="yes"/>

<xsl:template match="node()|@*">
	<xsl:copy>
		<xsl:apply-templates select="node()|@*"/>
	</xsl:copy>
</xsl:template>

<xsl:template match="xhtml:html">
		<xsl:apply-templates select="node()"/>
</xsl:template>

<xsl:template match="xhtml:head">
</xsl:template>

<xsl:template match="xhtml:body">
		<xsl:apply-templates select="node()"/>
</xsl:template>

<xsl:template match="xhtml:div[@class='top-right']">
	<div class="top-right"> 
		<span class="chapter"/> 
	</div>
</xsl:template>

<xsl:template match="xhtml:div[@class='footer']">
</xsl:template>

<xsl:template match="xhtml:h1/xhtml:a[@id]">
	<span>
		<xsl:attribute name="id">
			<xsl:value-of select="@id"/>
		</xsl:attribute> 
		<xsl:value-of select="."/>
	</span>
</xsl:template>

<xsl:template match="xhtml:h2/xhtml:a[@id]">
	<span>
		<xsl:attribute name="id">
			<xsl:value-of select="@id"/>
		</xsl:attribute> 
		<xsl:value-of select="."/>
	</span>
</xsl:template>

<xsl:template match="xhtml:h3/xhtml:a[@id]">
	<span>
		<xsl:attribute name="id">
			<xsl:value-of select="@id"/>
		</xsl:attribute> 
		<xsl:value-of select="."/>
	</span>
</xsl:template>

</xsl:stylesheet>
