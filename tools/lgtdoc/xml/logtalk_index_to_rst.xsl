<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  XSLT stylesheet for converting XML documenting files into
%  reStructuredText files for use with Sphinx
%
%  Last updated on Friday 13, 2023
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


<xsl:param name="mapping"/>


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
		<xsl:text>Libraries</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>=========</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>To load any library (including developer tools, ports, and contributions), use the goal ``logtalk_load(library_name(loader))``. To run the library tests, use the goal ``logtalk_load(library_name(tester))``. To load an entity, always load the loader file of the library that includes it to ensure that all required dependencies are also loaded and that any required flags are used. The loading goal can be found in the entity documentation.</xsl:text>
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:if test=".='directory'">
		<xsl:text>.. _directory_index:</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>Directories</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>===========</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>To load an entity, always load the library that includes it using the goal ``logtalk_load(library_name(loader))`` instead of using its path. The library loader file ensures that all the required dependencies are also loaded and that any required flags are used. The loading goal can be found in the entity documentation.</xsl:text>
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:if test=".='entity'">
		<xsl:text>.. _entity_index:</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>Entities</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>========</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>To load an entity, always load the library that includes it using the goal ``logtalk_load(library_name(loader))`` instead of loading just the entity. The library loader file ensures that all the required dependencies are also loaded and that any required flags are used. The loading goal can be found in the entity documentation.</xsl:text>
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:if test=".='predicate'">
		<xsl:text>.. _predicate_index:</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>Predicates</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>==========</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>This index lists all entities *declaring* a given predicate. To load an entity providing the predicate that you want to call, always load the library that includes it using the goal ``logtalk_load(library_name(loader))`` instead of loading just the entity. The library loader file ensures that all the required dependencies are also loaded and that any required flags are used. The loading goal can be found in the entity documentation.</xsl:text>
		<xsl:value-of select="$nl2" />
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
				<xsl:text>* :ref:`</xsl:text><xsl:value-of select="name" /><xsl:text> &lt;</xsl:text><xsl:value-of select="functor" />::<xsl:value-of select="../../key" /><xsl:text>&gt;`</xsl:text>
				<xsl:value-of select="$nl" />
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<xsl:for-each select="entities/entity">
				<xsl:text>   </xsl:text><xsl:value-of select="name" /><xsl:text> &lt;</xsl:text><xsl:value-of select="file" /><xsl:text>&gt;</xsl:text>
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
			<xsl:value-of select="$nl2" />
			<xsl:text>.. toctree::</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>   :maxdepth: 1</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:when test=".='protocol'">
			<xsl:text>Protocols</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>---------</xsl:text>
			<xsl:value-of select="$nl2" />
			<xsl:text>.. toctree::</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>   :maxdepth: 1</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:when test=".='category'">
			<xsl:text>Categories</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>----------</xsl:text>
			<xsl:value-of select="$nl2" />
			<xsl:text>.. toctree::</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>   :maxdepth: 1</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:choose>
				<xsl:when test="($mapping != '') and (/logtalk_index/type='library')">
					<xsl:text>:ref:`</xsl:text><xsl:value-of select="." /><xsl:text> &#60;</xsl:text><xsl:value-of select="$mapping"/><xsl:text>:library_</xsl:text><xsl:value-of select="." /><xsl:text>&#62;`</xsl:text>
					<xsl:value-of select="$nl" />
					<xsl:call-template name="adornment">
						<xsl:with-param name="char" select="'-'"/>
						<xsl:with-param name="n" select="6 + string-length(.) + 2 + string-length($mapping) + 9 + string-length(.) + 2"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="/logtalk_index/type='library'">
					<xsl:text>:ref:`</xsl:text><xsl:value-of select="." /><xsl:text> &#60;library_</xsl:text><xsl:value-of select="." /><xsl:text>&#62;`</xsl:text>
					<xsl:value-of select="$nl" />
					<xsl:call-template name="adornment">
						<xsl:with-param name="char" select="'-'"/>
						<xsl:with-param name="n" select="6 + string-length(.) + 10 + string-length(.) + 2"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:otherwise>
					<xsl:text>``</xsl:text><xsl:value-of select="." /><xsl:text>``</xsl:text>
					<xsl:value-of select="$nl" />
					<xsl:call-template name="adornment">
						<xsl:with-param name="char" select="'-'"/>
						<xsl:with-param name="n" select="2 + string-length(.) + 2"/>
					</xsl:call-template>
				</xsl:otherwise>
			</xsl:choose>
			<xsl:value-of select="$nl2" />
			<xsl:text>.. toctree::</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>   :maxdepth: 1</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


</xsl:stylesheet>
