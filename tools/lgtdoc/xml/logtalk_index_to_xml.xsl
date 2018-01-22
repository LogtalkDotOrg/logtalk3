<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<xsl:output
	method="html"
	version="4.0"
    indent="yes"
    encoding="utf-8"
	doctype-public="-//W3C//DTD HTML 4.01//EN"
	doctype-system="http://www.w3.org/TR/html4/strict.dtd"/>


<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  XSLT stylesheet for viewing XML documenting files in a browser
%  Last updated on July 5, 2016
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


<xsl:template match="/">
	<html>
	<head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8" />
		<title><xsl:apply-templates select="logtalk_index/type" /></title>
		<link rel="stylesheet" href="logtalk.css" type="text/css" />
	</head>
	<body>
		<div class="header">
			<h1 class="code"><xsl:apply-templates select="logtalk_index/type" /></h1>
		</div>
		<div class="predicates">
			<xsl:apply-templates select="logtalk_index/entries" />
		</div>
	</body>
	</html>
</xsl:template>


<xsl:template match="logtalk_index/type">
	<xsl:if test=".='library'">
		Library index
	</xsl:if>
	<xsl:if test=".='directory'">
		Directory index
	</xsl:if>
	<xsl:if test=".='entity'">
		Entity index
	</xsl:if>
	<xsl:if test=".='predicate'">
		Predicate index
	</xsl:if>
</xsl:template>


<xsl:template match="logtalk_index/entries">
	<xsl:apply-templates select="entry" />
</xsl:template>


<xsl:template match="*/entry">
	<dl>
	<dt><code><xsl:apply-templates select="key" /></code></dt>
		<xsl:for-each select="entities/entity">
			<dd class ="code"><code><a href="{file}.xml"><xsl:value-of select="name" /></a></code></dd>
		</xsl:for-each>
	</dl>
</xsl:template>


<xsl:template match="*/key">
	<xsl:choose>
	    <xsl:when test=".='object'">
			Objects
		</xsl:when>
	    <xsl:when test=".='protocol'">
			Protocols
		</xsl:when>
	    <xsl:when test=".='category'">
			Categories
		</xsl:when>
		<xsl:otherwise>
			<xsl:value-of select="." />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


</xsl:stylesheet>
