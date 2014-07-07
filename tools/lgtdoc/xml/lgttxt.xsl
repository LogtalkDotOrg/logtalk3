<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	<xsl:value-of select="logtalk/entity/type" /><xsl:text>: </xsl:text><xsl:value-of select="logtalk/entity/name" /><xsl:value-of select="$nl" />
	<xsl:if test="logtalk/entity/comment">
		<xsl:value-of select="$tab" /><xsl:value-of select="logtalk/entity/comment" /><xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:if test="logtalk/entity/parameters">
		<xsl:for-each select="logtalk/entity/parameters/parameter">
			<xsl:text>  * </xsl:text><xsl:value-of select="name" /><xsl:text> - </xsl:text><xsl:value-of select="description" />
		</xsl:for-each>
	</xsl:if>
	<xsl:apply-templates select="logtalk/entity" />
	<xsl:apply-templates select="logtalk/relations" />
	<xsl:apply-templates select="logtalk/predicates" />
	<xsl:apply-templates select="logtalk/operators" />
	<xsl:apply-templates select="logtalk/remarks" />
	<xsl:value-of select="$hr1" />
</xsl:template>


<xsl:template match="logtalk/entity">
	<xsl:if test="author">
		<xsl:value-of select="$tab" /><xsl:text>author: </xsl:text><xsl:value-of select="author" /><xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="version">
		<xsl:value-of select="$tab" /><xsl:text>version: </xsl:text><xsl:value-of select="version" /><xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="date">
		<xsl:value-of select="$tab" /><xsl:text>date: </xsl:text><xsl:value-of select="date" /><xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="copyright">
		<xsl:value-of select="$tab" /><xsl:text>copyright: </xsl:text><xsl:value-of select="copyright" /><xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="license">
		<xsl:value-of select="$tab" /><xsl:text>license: </xsl:text><xsl:value-of select="license" /><xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:value-of select="$tab" /><xsl:text>compilation flags: </xsl:text><xsl:value-of select="compilation" /><xsl:value-of select="$nl" />
	<xsl:if test="info">
		<xsl:for-each select="info">
			<xsl:value-of select="$tab" /><xsl:value-of select="key" /><xsl:text>: </xsl:text><xsl:value-of select="value" /><xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk/relations">
	<xsl:choose>
		<xsl:when test="*">
			<xsl:if test="implements">
				<xsl:text>implements:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="implements" />
			</xsl:if>
			<xsl:if test="imports">
				<xsl:text>imports:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="imports" />
			</xsl:if>
			<xsl:if test="extends">
				<xsl:text>extends:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="extends" />
			</xsl:if>
			<xsl:if test="instantiates">
				<xsl:text>instantiates:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="instantiates" />
			</xsl:if>
			<xsl:if test="specializes">
				<xsl:text>specializes:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="specializes" />
			</xsl:if>
			<xsl:if test="provides">
				<xsl:text>provides:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="provides" />
			</xsl:if>
			<xsl:if test="uses">
				<xsl:text>uses:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="uses" />
			</xsl:if>
			<xsl:if test="calls">
				<xsl:text>calls:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="calls" />
			</xsl:if>
			<xsl:if test="alias">
				<xsl:text>aliases:</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="alias" />
			</xsl:if>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(no dependencies on other files)</xsl:text><xsl:value-of select="$nl" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk/relations/provides" priority="1">
	<xsl:value-of select="$tab" /><xsl:value-of select="name" /><xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk/relations/uses" priority="1">
	<xsl:value-of select="$tab" /><xsl:value-of select="name" /><xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk/relations/calls" priority="1">
	<xsl:value-of select="$tab" /><xsl:value-of select="name" /><xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk/relations/alias" priority="1">
	<xsl:value-of select="$tab" /><xsl:value-of select="name" /><xsl:text> </xsl:text><xsl:value-of select="original" /><xsl:text> aka </xsl:text><xsl:value-of select="alternative" />
</xsl:template>


<xsl:template match="logtalk/relations/*" priority="0">
	<xsl:value-of select="$tab" /><xsl:value-of select="scope" /><xsl:text> </xsl:text><xsl:value-of select="name" /><xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk/predicates">
	<xsl:value-of select="$hr2" />
	<xsl:text>Public interface</xsl:text><xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">
			<xsl:text>(see related entities)</xsl:text><xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text><xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="$hr2" />
	<xsl:text>Protected interface</xsl:text><xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">
			<xsl:text>(see related entities)</xsl:text><xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text><xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="$hr2" />
	<xsl:text>Private predicates</xsl:text><xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">
			<xsl:text>(see related entities)</xsl:text><xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text><xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="*/predicate">
	<xsl:value-of select="name" /><xsl:value-of select="$nl" />
	<xsl:if test="comment">
		<xsl:value-of select="$tab" /><xsl:value-of select="comment" /><xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:text>compilation flags: </xsl:text><xsl:value-of select="$nl" />
	<xsl:value-of select="$tab" /><xsl:value-of select="compilation" /><xsl:value-of select="$nl" />
	<xsl:if test="template">
		<xsl:text>template: </xsl:text><xsl:value-of select="$nl" />
		<xsl:value-of select="$tab" /><xsl:value-of select="template" /><xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="arguments">
		<xsl:for-each select="arguments/argument">
			<xsl:value-of select="$tab2" /><xsl:value-of select="name" /><xsl:text> - </xsl:text><xsl:value-of select="description" /><xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="meta">
		<xsl:text>meta-predicate template:</xsl:text><xsl:value-of select="$nl" />
		<xsl:value-of select="$tab" /><xsl:value-of select="meta" /><xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="coinductive">
		<xsl:text>coinduction predicate template:</xsl:text><xsl:value-of select="$nl" />
		<xsl:value-of select="$tab" /><xsl:value-of select="coinductive" /><xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="mode">
		<xsl:text>mode - number of solutions:</xsl:text><xsl:value-of select="$nl" />
		<xsl:for-each select="mode">
			<xsl:value-of select="$tab" /><xsl:value-of select="template" /><xsl:text> - </xsl:text><xsl:value-of select="solutions" /><xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="exceptions">
		<xsl:text>exceptions:</xsl:text><xsl:value-of select="$nl" />
		<xsl:for-each select="exceptions/exception">
			<xsl:value-of select="$tab" /><xsl:value-of select="condition" /><xsl:text>: </xsl:text><xsl:value-of select="term" /><xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="examples">
		<xsl:text>examples:</xsl:text><xsl:value-of select="$nl" />
		<xsl:for-each select="examples/example">
			<xsl:value-of select="$tab" /><xsl:value-of select="description" /><xsl:value-of select="$nl" />
				<xsl:value-of select="$tab2" /><xsl:value-of select="call" /><xsl:value-of select="$nl" />
				<xsl:value-of select="$tab2" /><xsl:value-of select="bindings" /><xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="info">
		<xsl:for-each select="info">
			<xsl:value-of select="key" /><xsl:text>:</xsl:text><xsl:value-of select="$nl" />
			<xsl:value-of select="$tab" /><xsl:value-of select="value" /><xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk/operators">
	<xsl:value-of select="$hr2" />
	<xsl:text>Operators</xsl:text><xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="operator">
			<xsl:text>local operator declarations:</xsl:text><xsl:value-of select="$nl" />
			<xsl:for-each select="operator">
				<xsl:value-of select="$tab" /><xsl:value-of select="term" /><xsl:text> (</xsl:text><xsl:value-of select="scope" />)<xsl:value-of select="$nl" />
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text><xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk/remarks">
	<xsl:value-of select="$hr2" />
	<xsl:text>Remarks</xsl:text><xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="remark">
			<xsl:apply-templates select="remark" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text><xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="logtalk/remarks/remark">
	<xsl:value-of select="topic" /><xsl:text> </xsl:text><xsl:value-of select="text" /><xsl:value-of select="$nl2" />
</xsl:template>


</xsl:stylesheet>
