<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  XSLT stylesheet for converting XML documenting files into Markdown files
%  Last updated on March 14, 2016
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
	<xsl:text>&#10;-------------------------------------------------------------------------------&#10;</xsl:text>
</xsl:variable>


<xsl:template match="/">
	<xsl:value-of select="$hr" />
	<xsl:text># </xsl:text><xsl:value-of select="logtalk_entity/entity/type" /><xsl:text>: `</xsl:text><xsl:value-of select="logtalk_entity/entity/name" /><xsl:text>`</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:if test="logtalk_entity/entity/comment">
		<xsl:value-of select="logtalk_entity/entity/comment" />
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:if test="logtalk_entity/entity/parameters">
		<xsl:for-each select="logtalk_entity/entity/parameters/parameter">
			<xsl:text>* `</xsl:text><xsl:value-of select="name" /><xsl:text>` - </xsl:text><xsl:value-of select="description" />
		</xsl:for-each>
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:apply-templates select="logtalk_entity/entity" />
	<xsl:apply-templates select="logtalk_entity/relations" />
	<xsl:apply-templates select="logtalk_entity/predicates" />
	<xsl:apply-templates select="logtalk_entity/operators" />
	<xsl:apply-templates select="logtalk_entity/remarks" />
	<xsl:apply-templates select="logtalk_entity/see_also" />
	<xsl:value-of select="$hr" />
</xsl:template>


<xsl:template match="logtalk_entity/entity">
	<xsl:if test="author">
		<xsl:text>* author: </xsl:text><xsl:value-of select="author" />
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="version">
		<xsl:text>* version: </xsl:text><xsl:value-of select="version" />
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="date">
		<xsl:text>* date: </xsl:text><xsl:value-of select="date" />
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="copyright">
		<xsl:text>* copyright: </xsl:text><xsl:value-of select="copyright" />
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="license">
		<xsl:text>* license: </xsl:text><xsl:value-of select="license" />
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:text>* compilation flags: `</xsl:text><xsl:value-of select="compilation" /><xsl:text>`</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:if test="info">
		<xsl:for-each select="info">
			<xsl:text>* </xsl:text><xsl:value-of select="key" /><xsl:text>: </xsl:text><xsl:value-of select="value" />
			<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations">
	<xsl:choose>
		<xsl:when test="*">
			<xsl:if test="implements">
				<xsl:text>* implements:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="implements" />
			</xsl:if>
			<xsl:if test="imports">
				<xsl:text>* imports:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="imports" />
			</xsl:if>
			<xsl:if test="extends">
				<xsl:text>* extends:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="extends" />
			</xsl:if>
			<xsl:if test="instantiates">
				<xsl:text>* instantiates:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="instantiates" />
			</xsl:if>
			<xsl:if test="specializes">
				<xsl:text>* specializes:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="specializes" />
			</xsl:if>
			<xsl:if test="provides">
				<xsl:text>* provides:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="provides" />
			</xsl:if>
			<xsl:if test="uses">
				<xsl:text>* uses:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="uses" />
			</xsl:if>
			<xsl:if test="calls">
				<xsl:text>* calls:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="calls" />
			</xsl:if>
			<xsl:if test="alias">
				<xsl:text>* aliases:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="alias" />
			</xsl:if>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(no dependencies on other files)</xsl:text><xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/provides" priority="1">
	<xsl:text>  * [`</xsl:text><xsl:value-of select="to" />::<xsl:value-of select="resource" /><xsl:text>`](</xsl:text><xsl:value-of select="file" /><xsl:text>.md#</xsl:text><xsl:value-of select="resource" /><xsl:text>)</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/uses" priority="1">
	<xsl:text>  * [`</xsl:text><xsl:value-of select="name" /><xsl:text>`](</xsl:text><xsl:value-of select="file" /><xsl:text>.md)</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/calls" priority="1">
	<xsl:text>  * [`</xsl:text><xsl:value-of select="name" /><xsl:text>`](</xsl:text><xsl:value-of select="file" /><xsl:text>.md)</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/alias" priority="1">
	<xsl:text>  * `</xsl:text><xsl:value-of select="name" /><xsl:text>` `</xsl:text><xsl:value-of select="original" /><xsl:text>` aka `</xsl:text><xsl:value-of select="alternative" /><xsl:text>`</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/*" priority="0">
	<xsl:text>  * `</xsl:text><xsl:value-of select="scope" /><xsl:text>` [`</xsl:text><xsl:value-of select="name" /><xsl:text>`](</xsl:text><xsl:value-of select="file" /><xsl:text>.md)</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/predicates">
	<xsl:text>## Public interface</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
			<xsl:text>(see related entities)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:text>## Protected interface</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
			<xsl:text>(see related entities)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:text>## Private predicates</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
			<xsl:text>(see related entities)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="*/predicate">
	<xsl:text>### `</xsl:text><xsl:value-of select="name" /><xsl:text>`</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:if test="comment">
		<xsl:value-of select="$nl" />
		<xsl:value-of select="comment" />
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:text>* compilation flags: `</xsl:text><xsl:value-of select="compilation" /><xsl:text>`</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:if test="template">
		<xsl:text>* template: `</xsl:text><xsl:value-of select="template" /><xsl:text>`</xsl:text>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="arguments">
		<xsl:for-each select="arguments/argument">
			<xsl:text>  * `</xsl:text><xsl:value-of select="name" /><xsl:text>` - </xsl:text><xsl:value-of select="description" />
			<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="meta">
		<xsl:text>* meta-predicate template:</xsl:text><xsl:value-of select="meta" /><xsl:text>`</xsl:text>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="coinductive">
		<xsl:text>* coinduction predicate template: `</xsl:text><xsl:value-of select="coinductive" /><xsl:text>`</xsl:text>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="mode">
		<xsl:text>* mode - number of proofs:</xsl:text><xsl:value-of select="$nl" />
		<xsl:for-each select="mode">
			<xsl:text>  * `</xsl:text><xsl:value-of select="template" /><xsl:text>` - `</xsl:text><xsl:value-of select="proofs" /><xsl:text>`</xsl:text>
			<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="exceptions">
		<xsl:text>* exceptions:</xsl:text><xsl:value-of select="$nl" />
		<xsl:for-each select="exceptions/exception">
			<xsl:text>  * </xsl:text><xsl:value-of select="condition" /><xsl:text>: `</xsl:text><xsl:value-of select="term" /><xsl:text>`</xsl:text>
			<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="remarks">
		<xsl:text>* remarks:</xsl:text><xsl:value-of select="$nl" />
		<xsl:for-each select="remarks/remark">
			<xsl:text>  * </xsl:text><xsl:value-of select="topic" /><xsl:text>: </xsl:text><xsl:value-of select="text" />
			<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="examples">
		<xsl:text>* examples:</xsl:text><xsl:value-of select="$nl" />
		<xsl:for-each select="examples/example">
			<xsl:text>  * </xsl:text><xsl:value-of select="description" />
			<xsl:value-of select="$nl" />
				<xsl:text>    * `</xsl:text><xsl:value-of select="call" /><xsl:text>`</xsl:text>
				<xsl:value-of select="$nl" />
				<xsl:text>    * `</xsl:text><xsl:value-of select="bindings" /><xsl:text>`</xsl:text>
				<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="info">
		<xsl:for-each select="info">
			<xsl:text>* </xsl:text><xsl:value-of select="key" /><xsl:text>: </xsl:text><xsl:value-of select="value" />
			<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/operators">
	<xsl:text>## Operators</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="operator">
			<xsl:for-each select="operator">
				<xsl:text>* `</xsl:text><xsl:value-of select="term" /><xsl:text> (</xsl:text><xsl:value-of select="scope" /><xsl:text>)`</xsl:text>
				<xsl:value-of select="$nl" />
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk_entity/remarks">
	<xsl:text>## Remarks</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="remark">
			<xsl:apply-templates select="remark" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="logtalk_entity/remarks/remark">
	<xsl:text>* </xsl:text><xsl:value-of select="topic" /><xsl:text>: </xsl:text><xsl:value-of select="text" />
	<xsl:value-of select="$nl2" />
</xsl:template>


<xsl:template match="logtalk_entity/see_also">
	<xsl:text>## See also</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="reference">
			<xsl:apply-templates select="reference" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="logtalk_entity/see_also/reference">
	<xsl:text>* [`</xsl:text><xsl:value-of select="name" /><xsl:text>`](</xsl:text><xsl:value-of select="file" /><xsl:text>.md)</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


</xsl:stylesheet>
