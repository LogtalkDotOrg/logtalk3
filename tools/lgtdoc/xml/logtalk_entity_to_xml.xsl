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
%  Last updated on August 11, 2017
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


<xsl:template match="/">
	<html>
	<head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8" />
		<title><xsl:value-of select="logtalk_entity/entity/name" /></title>
		<link rel="stylesheet" href="logtalk.css" type="text/css" />
	</head>
	<body>
		<div class="header">
			<p class="type"><xsl:value-of select="logtalk_entity/entity/type" /></p>
			<h1 class="code"><xsl:value-of select="logtalk_entity/entity/name" /></h1>
			<blockquote>
			<xsl:if test="logtalk_entity/entity/comment">
				<p class="comment"><xsl:value-of select="logtalk_entity/entity/comment" /></p>
			</xsl:if>
			<xsl:if test="logtalk_entity/entity/parameters">
				<ul class="parameters">
				<xsl:for-each select="logtalk_entity/entity/parameters/parameter">
					<li><code><xsl:value-of select="name" /></code><xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text><span class="comment"><xsl:value-of select="description" /></span></li>
				</xsl:for-each>
				</ul>
			</xsl:if>
			</blockquote>
		</div>
		<div class="entity">
			<div class="section">
				<xsl:apply-templates select="logtalk_entity/entity" />
				<xsl:apply-templates select="logtalk_entity/relations" />
			</div>
		</div>
		<div class="predicates">
			<xsl:apply-templates select="logtalk_entity/predicates" />
		</div>
		<div class="operators">
			<xsl:apply-templates select="logtalk_entity/operators" />
		</div>
		<div class="remarks">
			<xsl:apply-templates select="logtalk_entity/remarks" />
		</div>
		<div class="see_also">
			<xsl:apply-templates select="logtalk_entity/see_also" />
		</div>
	</body>
	</html>
</xsl:template>


<xsl:template match="logtalk_entity/entity">
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


<xsl:template match="logtalk_entity/relations">
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
			<xsl:if test="provides">
			<dt class ="key">provides:</dt>
				<xsl:apply-templates select="provides" />
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
			<p class="comment">(no dependencies on other entities)</p>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk_entity/relations/provides" priority="1">
	<dd class ="value"><code><a href="{file}.xml"><xsl:value-of select="to" />::<xsl:value-of select="resource" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk_entity/relations/uses" priority="1">
	<dd class ="value"><code><a href="{file}.xml"><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk_entity/relations/calls" priority="1">
	<dd class ="value"><code><a href="{file}.xml"><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk_entity/relations/alias" priority="1">
	<dd class ="value"><code><xsl:value-of select="name" /><xsl:text> </xsl:text><xsl:value-of select="original" /></code><em> aka </em><code><xsl:value-of select="alternative" /></code></dd>
</xsl:template>


<xsl:template match="logtalk_entity/relations/*" priority="0">
	<dd class ="value"><code><xsl:value-of select="scope" /><xsl:text> </xsl:text><a href="{file}.xml"><xsl:value-of select="name" /></a></code></dd>
</xsl:template>


<xsl:template match="logtalk_entity/predicates">
	<div class="public">
	<h2>Public interface</h2>
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
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
		<xsl:when test="/logtalk_entity/relations/*">
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
		<xsl:when test="/logtalk_entity/relations/*">
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
		<xsl:if test="coinductive">
		<dt class ="key">coinductive predicate template:</dt>
			<dd class ="value"><code><xsl:value-of select="coinductive" /></code></dd>
		</xsl:if>
		<xsl:if test="mode">
		<dt class ="key">mode<xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text>number of proofs:</dt>
		<xsl:for-each select="mode">
			<dd class ="value"><code><xsl:value-of select="template" /><xsl:text disable-output-escaping="yes"> &amp;ndash; </xsl:text><xsl:value-of select="proofs" /></code></dd>
		</xsl:for-each>
		</xsl:if>
		<xsl:if test="exceptions">
		<dt class ="key">exceptions:</dt>
		<xsl:for-each select="exceptions/exception">
			<dd class ="value"><xsl:value-of select="condition" />: <code><xsl:value-of select="term" /></code></dd>
		</xsl:for-each>
		</xsl:if>
		<xsl:if test="remarks">
		<dt class ="key">remarks:</dt>
		<xsl:for-each select="remarks/remark">
			<dd class ="value"><xsl:value-of select="topic" />: <xsl:value-of select="text" /></dd>
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


<xsl:template match="logtalk_entity/operators">
	<h2>Operators</h2>
	<div class="section">
	<xsl:choose>
		<xsl:when test="operator">
			<xsl:for-each select="operator">
				<h3 class="code"><xsl:value-of select="term" /> (<xsl:value-of select="scope" />)</h3>
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<h3 class="comment">(none)</h3>
		</xsl:otherwise>
	</xsl:choose>
	</div>
</xsl:template>


<xsl:template match="logtalk_entity/remarks">
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

<xsl:template match="logtalk_entity/remarks/remark">
	<dl class="remarks">
		<dt class="comment"><xsl:value-of select="topic" /></dt>
			<dd class="text"><xsl:value-of select="text" /></dd>
	</dl>
</xsl:template>


<xsl:template match="logtalk_entity/see_also">
	<h2>See also</h2>
	<div class="section">
	<xsl:choose>
		<xsl:when test="reference">
			<ul>
				<xsl:apply-templates select="reference" />
			</ul>
		</xsl:when>
		<xsl:otherwise>
			<h3 class="comment">(none)</h3>
		</xsl:otherwise>
	</xsl:choose>
	</div>
</xsl:template>

<xsl:template match="logtalk_entity/see_also/reference">
	<li class ="code"><a href="{file}.html"><xsl:value-of select="name" /></a></li>
</xsl:template>


</xsl:stylesheet>
