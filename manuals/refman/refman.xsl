<?xml version="1.0" encoding="utf-8"?>

<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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

<xsl:template match="xhtml:h4/xhtml:a[@id]">
	<span>
		<xsl:attribute name="id">
			<xsl:value-of select="@id"/>
		</xsl:attribute> 
		<xsl:value-of select="."/>
	</span>
</xsl:template>

</xsl:stylesheet>
