<?xml version="1.0"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/TR/WD-xsl">


<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
% 
%   XSLT stylesheet for viewing XML documenting files in a browser
%   Last updated on November 3, 2014
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


<xsl:template match="/">
	<html>
	<head>
		<title><xsl:value-of select="logtalk/entity/name" /></title>
		<link rel="stylesheet" href="logtalk.css" type="text/css" />
	</head>
	<body>
		<hr />
		<h4 class="type"><xsl:value-of select="logtalk/entity/type" /></h4>
		<h1 class="code"><xsl:value-of select="logtalk/entity/name" /></h1>
		<xsl:apply-templates select="logtalk/entity" />
		<hr />
		<xsl:apply-templates select="logtalk/relations" />
		<hr />
		<xsl:apply-templates select="logtalk/predicates" />
		<hr />
	</body>
	</html>
</xsl:template>


<xsl:template match="logtalk/entity">
	<xsl:if test="comment">
		<blockquote><xsl:value-of select="comment" /></blockquote>
	</xsl:if>
	<dl>
	<xsl:if test="author">
		<dt>author:</dt>
			<dd><code><xsl:value-of select="author" /></code></dd>
	</xsl:if>
	<xsl:if test="version">
		<dt>version:</dt>
			<dd><code><xsl:value-of select="version" /></code></dd>
	</xsl:if>
	<xsl:if test="date">
		<dt>date:</dt>
			<dd><code><xsl:value-of select="date" /></code></dd>
	</xsl:if>
	</dl>
	<dl>
		<dt>compilation flags:</dt>
			<dd><code><xsl:value-of select="compilation" /></code></dd>
	</dl>
	<dl>
	<xsl:for-each select="info">
		<dt><xsl:value-of select="key" />:</dt>
			<dd><code><xsl:value-of select="value" /></code></dd>
	</xsl:for-each>
	</dl>
</xsl:template>


<xsl:template match="logtalk/relations">
	<xsl:choose>
		<xsl:when test="*">
			<xsl:if test="implements">
			<dl>
			<dt>implements:</dt>
				<xsl:apply-templates select="implements" />
			</dl>
			</xsl:if>
			<xsl:if test="imports">
			<dl>
			<dt>imports:</dt>
				<xsl:apply-templates select="imports" />
			</dl>
			</xsl:if>
			<xsl:if test="extends">
			<dl>
			<dt>extends:</dt>
				<xsl:apply-templates select="extends" />
			</dl>
			</xsl:if>
			<xsl:if test="instantiates">
			<dl>
			<dt>instantiates:</dt>
				<xsl:apply-templates select="instantiates" />
			</dl>
			</xsl:if>
			<xsl:if test="specializes">
			<dl>
			<dt>specializes:</dt>
				<xsl:apply-templates select="specializes" />
			</dl>
			</xsl:if>
			<xsl:if test="uses">
			<dl>
			<dt>uses:</dt>
				<xsl:apply-templates select="uses" />
			</dl>
			</xsl:if>
			<xsl:if test="calls">
			<dl>
			<dt>calls:</dt>
				<xsl:apply-templates select="calls" />
			</dl>
			</xsl:if>
		</xsl:when>
		<xsl:otherwise>
			<h4 class="code">(no dependencies on other files)</h4>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk/relations/uses">
	<dd><code><a><xsl:attribute name="href"><xsl:value-of select="file" />.xml</xsl:attribute><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/relations/calls">
	<dd><code><a><xsl:attribute name="href"><xsl:value-of select="file" />.xml</xsl:attribute><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/relations/*" xml:space="preserve">
	<dd><code><xsl:value-of select="scope" /> <a><xsl:attribute name="href"><xsl:value-of select="file" />.xml</xsl:attribute><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/predicates">
	<h1>Public interface</h1>
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">
			<h4 class="code">(see related entities)</h4>
		</xsl:when>
		<xsl:otherwise>
			<h4 class="code">(none)</h4>
		</xsl:otherwise>
	</xsl:choose>
	<h1>Protected interface</h1>
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">
			<h4 class="code">(see related entities)</h4>
		</xsl:when>
		<xsl:otherwise>
			<h4 class="code">(none)</h4>
		</xsl:otherwise>
	</xsl:choose>
	<h1>Private predicates</h1>
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">
			<h4 class="code">(see related entities)</h4>
		</xsl:when>
		<xsl:otherwise>
			<h4 class="code">(none)</h4>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="predicate">
	<h4 class="code"><xsl:value-of select="name" /></h4>
	<xsl:if test="comment">
		<blockquote><xsl:value-of select="comment" /></blockquote>
	</xsl:if>
	<dl class="predicate">
		<dt>compilation flags:</dt>
			<dd><code><xsl:value-of select="compilation" /></code></dd>
		<xsl:if test="template">
		<dt>template:</dt>
			<dd><code><xsl:value-of select="template" /></code></dd>
		</xsl:if>
		<xsl:if test="meta">
		<dt>meta-predicate template:</dt>
			<dd><code><xsl:value-of select="meta" /></code></dd>
		</xsl:if>
		<xsl:if test="mode">
		<dt>mode - number of proofs:</dt>
		<xsl:for-each select="mode" xml:space="preserve">
			<dd><code><xsl:value-of select="template" /> - <xsl:value-of select="proofs" /></code></dd>
		</xsl:for-each>
		</xsl:if>
		<xsl:if test="exceptions">
		<dt>exceptions:</dt>
		<xsl:for-each select="exceptions/exception">
			<dd><xsl:value-of select="condition" />: <code><xsl:value-of select="term" /></code></dd>
		</xsl:for-each>
		</xsl:if>
	</dl>
	<dl class="predicate">
		<xsl:for-each select="info">
		<dt><xsl:value-of select="key" />:</dt>
			<dd><code><xsl:value-of select="value" /></code></dd>
		</xsl:for-each>
	</dl>
</xsl:template>


</xsl:stylesheet>
