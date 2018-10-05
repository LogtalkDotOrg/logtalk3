<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  XSLT stylesheet for converting XML documenting files into
%  reStructuredText files
%  Last updated on October 5, 2018
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

<xsl:variable name="hr">
	<xsl:text>&#10;&#10;--------&#10;&#10;</xsl:text>
</xsl:variable>

<xsl:template name="adornment">
	<xsl:param name="char"/>
	<xsl:param name="n"/>
	<xsl:if test="$n > 0">
		<xsl:call-template name="adornment">
			<xsl:with-param name="char" select="$char"/>
			<xsl:with-param name="n" select="$n - 1"/>
		</xsl:call-template>
		<xsl:value-of select="$char"/>
	</xsl:if>
</xsl:template>


<xsl:template match="/">
	<xsl:value-of select="$nl" />
	<xsl:apply-templates select="logtalk_index/type" />
	<xsl:value-of select="$nl2" />
	<xsl:apply-templates select="logtalk_index/entries" />
</xsl:template>


<xsl:template match="logtalk_index/type">
	<xsl:if test=".='library'">
		<xsl:text>.. _library_index:</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>Library index</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>=============</xsl:text>
	</xsl:if>
	<xsl:if test=".='directory'">
		<xsl:text>.. _directory_index:</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>Directory index</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>===============</xsl:text>
	</xsl:if>
	<xsl:if test=".='entity'">
		<xsl:text>.. _entity_index:</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>Entity index</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>============</xsl:text>
	</xsl:if>
	<xsl:if test=".='predicate'">
		<xsl:text>.. _predicate_index:</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>Predicate index</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>===============</xsl:text>
	</xsl:if>
</xsl:template>


<xsl:template match="logtalk_index/entries">
	<xsl:apply-templates select="entry" />
</xsl:template>


<xsl:template match="*/entry">
	<xsl:apply-templates select="key" />
	<xsl:value-of select="$nl2" />
	<xsl:choose>
	    <xsl:when test="/logtalk_index/type='predicate'">
			<xsl:for-each select="entities/entity">
				<xsl:text>* :ref:`</xsl:text><xsl:value-of select="name" /><xsl:text> &lt;</xsl:text><xsl:value-of select="functor" /><xsl:text>&gt;`</xsl:text>
				<xsl:value-of select="$nl" />
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<xsl:for-each select="entities/entity">
				<xsl:text>* :ref:`</xsl:text><xsl:value-of select="name" /><xsl:text> &lt;</xsl:text><xsl:value-of select="functor" /><xsl:text>&gt;`</xsl:text>
				<xsl:value-of select="$nl" />
			</xsl:for-each>
		</xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="*/key">
	<xsl:choose>
	    <xsl:when test=".='object'">
			<xsl:text>Objects</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>-------</xsl:text>		
		</xsl:when>
	    <xsl:when test=".='protocol'">
			<xsl:text>Protocols</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>---------</xsl:text>
		</xsl:when>
	    <xsl:when test=".='category'">
			<xsl:text>Categories</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>----------</xsl:text>
		</xsl:when>
		<xsl:otherwise>
			<xsl:value-of select="." />
			<xsl:value-of select="$nl" />
			<xsl:call-template name="adornment">
				<xsl:with-param name="char" select="'-'"/>
				<xsl:with-param name="n" select="string-length(.)"/>
			</xsl:call-template>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


</xsl:stylesheet>
