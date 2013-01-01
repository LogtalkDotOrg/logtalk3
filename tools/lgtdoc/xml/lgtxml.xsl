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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
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
        <meta http-equiv="content-type" content="text/html; charset=utf-8" />
		<title><xsl:value-of select="logtalk/entity/name" /></title>
		<link rel="stylesheet" href="logtalk.css" type="text/css" />
	</head>
	<body>
		<div class="header">
			<p class="type"><xsl:value-of select="logtalk/entity/type" /></p>
			<h1 class="code"><xsl:value-of select="logtalk/entity/name" /></h1>
			<blockquote>
			<xsl:if test="logtalk/entity/comment">
				<p class="comment"><xsl:value-of select="logtalk/entity/comment" /></p>
			</xsl:if>
			<xsl:if test="logtalk/entity/parameters">
				<ul class="parameters">
				<xsl:for-each select="logtalk/entity/parameters/parameter">
					<li><code><xsl:value-of select="name" /></code><xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text><span class="comment"><xsl:value-of select="description" /></span></li>
				</xsl:for-each>
				</ul>
			</xsl:if>
			</blockquote>
		</div>
		<div class="entity">
			<div class="section">
				<xsl:apply-templates select="logtalk/entity" />
				<xsl:apply-templates select="logtalk/relations" />
			</div>
		</div>
		<div class="predicates">
			<xsl:apply-templates select="logtalk/predicates" />
		</div>
		<div class="operators">
			<xsl:apply-templates select="logtalk/operators" />
		</div>
		<div class="remarks">
			<xsl:apply-templates select="logtalk/remarks" />
		</div>
	</body>
	</html>
</xsl:template>


<xsl:template match="logtalk/entity">
	<dl class="properties">
	<xsl:if test="author">
		<dt class ="key">author:</dt>
			<dd class="value"><code><xsl:value-of select="author" /></code></dd>
	</xsl:if>
	<xsl:if test="version">
		<dt class ="key">version:</dt>
			<dd class="value"><code><xsl:value-of select="version" /></code></dd>
	</xsl:if>
	<xsl:if test="date">
		<dt class ="key">date:</dt>
			<dd class="value"><code><xsl:value-of select="date" /></code></dd>
	</xsl:if>
	<xsl:if test="copyright">
		<dt class ="key">copyright:</dt>
			<dd class="value"><code><xsl:value-of select="copyright" /></code></dd>
	</xsl:if>
	<xsl:if test="license">
		<dt class ="key">license:</dt>
			<dd class="value"><code><xsl:value-of select="license" /></code></dd>
	</xsl:if>
	</dl>
	<dl class="properties">
		<dt class ="key">compilation flags:</dt>
			<dd class ="value"><code><xsl:value-of select="compilation" /></code></dd>
	</dl>
	<xsl:if test="info">
		<dl class="properties">
		<xsl:for-each select="info">
			<dt class ="key"><xsl:value-of select="key" />:</dt>
				<dd class ="value"><code><xsl:value-of select="value" /></code></dd>
		</xsl:for-each>
		</dl>
	</xsl:if>
</xsl:template>


<xsl:template match="logtalk/relations">
	<xsl:choose>
		<xsl:when test="*">
		<dl class="relations">
			<xsl:if test="implements">
			<dt class ="key">implements:</dt>
				<xsl:apply-templates select="implements" />
			</xsl:if>
			<xsl:if test="imports">
			<dt class ="key">imports:</dt>
				<xsl:apply-templates select="imports" />
			</xsl:if>
			<xsl:if test="extends">
			<dt class ="key">extends:</dt>
				<xsl:apply-templates select="extends" />
			</xsl:if>
			<xsl:if test="instantiates">
			<dt class ="key">instantiates:</dt>
				<xsl:apply-templates select="instantiates" />
			</xsl:if>
			<xsl:if test="specializes">
			<dt class ="key">specializes:</dt>
				<xsl:apply-templates select="specializes" />
			</xsl:if>
			<xsl:if test="uses">
			<dt class ="key">uses:</dt>
				<xsl:apply-templates select="uses" />
			</xsl:if>
			<xsl:if test="calls">
			<dt class ="key">calls:</dt>
				<xsl:apply-templates select="calls" />
			</xsl:if>
			<xsl:if test="alias">
			<dt class ="key">aliases:</dt>
				<xsl:apply-templates select="alias" />
			</xsl:if>
		</dl>
		</xsl:when>
		<xsl:otherwise>	
			<p class="comment">(no dependencies on other files)</p>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk/relations/uses" priority="1">
	<dd class ="value"><code><a href="{file}.xml"><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/relations/calls" priority="1">
	<dd class ="value"><code><a href="{file}.xml"><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/relations/alias" priority="1">
	<dd class ="value"><code><xsl:value-of select="name" /><xsl:text> </xsl:text><xsl:value-of select="original" /></code><em> aka </em><code><xsl:value-of select="alternative" /></code></dd>
</xsl:template>


<xsl:template match="logtalk/relations/*" priority="0">
	<dd class ="value"><code><xsl:value-of select="scope" /><xsl:text> </xsl:text><a href="{file}.xml"><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk/predicates">
	<div class="public">
	<h2>Public interface</h2>
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<div class="section">
				<p class="comment">(see related entities)</p>
			</div>
		</xsl:when>
		<xsl:otherwise>
			<div class="section">
				<p class="comment">(none)</p>
			</div>
		</xsl:otherwise>
	</xsl:choose>
	</div>
	<div class="protected">
	<h2>Protected interface</h2>
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<div class="section">
				<p class="comment">(see related entities)</p>
			</div>
		</xsl:when>
		<xsl:otherwise>
			<div class="section">
				<p class="comment">(none)</p>
			</div>
		</xsl:otherwise>
	</xsl:choose>
	</div>
	<div class="private">
	<h2>Private predicates</h2>
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk/relations/*">		
			<div class="section">
				<p class="comment">(see related entities)</p>
			</div>
		</xsl:when>
		<xsl:otherwise>
			<div class="section">
				<p class="comment">(none)</p>
			</div>
		</xsl:otherwise>
	</xsl:choose>
	</div>
</xsl:template>


<xsl:template match="*/predicate">
	<div class="section">
	<h3 class="code"><xsl:value-of select="name" /></h3>
	<blockquote>
	<xsl:if test="comment">
		<p class="comment"><xsl:value-of select="comment" /></p>
	</xsl:if>
	</blockquote>
	<dl class="properties">
		<dt class ="key">compilation flags:</dt>
			<dd class ="value"><code><xsl:value-of select="compilation" /></code></dd>
		<xsl:if test="template">
		<dt class ="key">template:</dt>
			<dd class ="value"><code><xsl:value-of select="template" /></code></dd>
		</xsl:if>
		<xsl:if test="arguments">
			<dd class ="value"><ul class="arguments">
			<xsl:for-each select="arguments/argument">
				<li><code><xsl:value-of select="name" /></code><xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text><span class="comment"><xsl:value-of select="description" /></span></li>
			</xsl:for-each></ul></dd>
		</xsl:if>
		<xsl:if test="meta">
		<dt class ="key">meta-predicate template:</dt>
			<dd class ="value"><code><xsl:value-of select="meta" /></code></dd>
		</xsl:if>
		<xsl:if test="mode">
		<dt class ="key">mode<xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text>number of solutions:</dt>
		<xsl:for-each select="mode">
			<dd class ="value"><code><xsl:value-of select="template" /><xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text><xsl:value-of select="solutions" /></code></dd>
		</xsl:for-each>
		</xsl:if>
		<xsl:if test="exceptions">
		<dt class ="key">exceptions:</dt>
		<xsl:for-each select="exceptions/exception">
			<dd class ="value"><xsl:value-of select="condition" />: <code><xsl:value-of select="term" /></code></dd>
		</xsl:for-each>
		</xsl:if>
		<xsl:if test="examples">
		<dt class ="key">examples:</dt>
			<xsl:for-each select="examples/example">
			<dd class ="value"><dl class="examples">
				<dt class="comment"><xsl:value-of select="description" /></dt>
					<dd class="code"><xsl:value-of select="call" /></dd>
					<dd class="code"><xsl:value-of select="bindings" /></dd>
			</dl></dd>
			</xsl:for-each>
		</xsl:if>
	</dl>
	<xsl:if test="info">
		<dl class="properties">
			<xsl:for-each select="info">
			<dt class ="key"><xsl:value-of select="key" />:</dt>
				<dd class ="value"><code><xsl:value-of select="value" /></code></dd>
			</xsl:for-each>
		</dl>
	</xsl:if>
	</div>
</xsl:template>


<xsl:template match="logtalk/operators">
	<h2>Operators</h2>
	<div class="section">
	<xsl:choose>
		<xsl:when test="operator">
			<dl class="properties">
				<dt class ="key">local operator declarations:</dt>
				<xsl:for-each select="operator">
					<dd class="code"><xsl:value-of select="term" /> (<xsl:value-of select="scope" />)</dd>
				</xsl:for-each>
			</dl>
		</xsl:when>
		<xsl:otherwise>
			<h3 class="comment">(none)</h3>
		</xsl:otherwise>
	</xsl:choose>
	</div>
</xsl:template>


<xsl:template match="logtalk/remarks">
	<h2>Remarks</h2>
	<div class="section">
	<xsl:choose>
		<xsl:when test="remark">
			<xsl:apply-templates select="remark" />
		</xsl:when>
		<xsl:otherwise>
			<h3 class="comment">(none)</h3>
		</xsl:otherwise>
	</xsl:choose>
	</div>
</xsl:template>

<xsl:template match="logtalk/remarks/remark">
	<dl class="remarks">
		<dt class="comment"><xsl:value-of select="topic" /></dt>
			<dd class="text"><xsl:value-of select="text" /></dd>
	</dl>
</xsl:template>


</xsl:stylesheet>
