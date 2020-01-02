<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  XSLT stylesheet for converting XML documenting files into text files
%
%  Last updated on July 5, 2016
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


<xsl:output
	method="text"
	indent="yes"
	encoding="utf-8"/>


<xsl:variable name="nl">
	<xsl:text>&#10;</xsl:text>
</xsl:variable>

<xsl:variable name="nl2">
	<xsl:text>&#10;&#10;</xsl:text>
</xsl:variable>

<xsl:variable name="tab">
	<xsl:text>&#9;</xsl:text>
</xsl:variable>

<xsl:variable name="tab2">
	<xsl:text>&#9;&#9;</xsl:text>
</xsl:variable>

<xsl:variable name="hr1">
	<xsl:text>===============================================================================&#10;</xsl:text>
</xsl:variable>

<xsl:variable name="hr2">
	<xsl:text>-------------------------------------------------------------------------------&#10;</xsl:text>
</xsl:variable>


<xsl:template match="/">
	<xsl:value-of select="$hr1" />
	<xsl:apply-templates select="logtalk_index/type" />
	<xsl:value-of select="$hr2" />
	<xsl:value-of select="$nl" />
	<xsl:apply-templates select="logtalk_index/entries" />
	<xsl:value-of select="$hr1" />
</xsl:template>


<xsl:template match="logtalk_index/type">
	<xsl:if test=".='library'">
		<xsl:text>Library index</xsl:text>
	</xsl:if>
	<xsl:if test=".='directory'">
		<xsl:text>Directory index</xsl:text>
	</xsl:if>
	<xsl:if test=".='entity'">
		<xsl:text>Entity index</xsl:text>
	</xsl:if>
	<xsl:if test=".='predicate'">
		<xsl:text>Predicate index</xsl:text>
	</xsl:if>
</xsl:template>


<xsl:template match="logtalk_index/entries">
	<xsl:apply-templates select="entry" />
</xsl:template>


<xsl:template match="*/entry">
	<xsl:apply-templates select="key" />
	<xsl:value-of select="$nl2" />
	<xsl:for-each select="entities/entity">
		<xsl:value-of select="$tab" /><xsl:value-of select="name" />
		<xsl:value-of select="$tab" /><xsl:value-of select="file" />
		<xsl:value-of select="$nl2" />
	</xsl:for-each>
	<xsl:value-of select="$hr2" />
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
