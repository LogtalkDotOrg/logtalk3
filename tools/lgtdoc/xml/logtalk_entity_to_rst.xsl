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
%  Last updated on November 20, 2019
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
	<xsl:text>.. index:: </xsl:text>
	<xsl:call-template name="replace-string">
		<xsl:with-param name="text" select="logtalk_entity/entity/name" />
		<xsl:with-param name="replace" select="','" />
		<xsl:with-param name="with" select="'&#xff0c;'" />
	</xsl:call-template>
	<xsl:value-of select="$nl" />
	<xsl:text>.. _</xsl:text><xsl:value-of select="logtalk_entity/entity/functor" /><xsl:text>:</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:text>.. rst-class:: right</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:text>**</xsl:text><xsl:value-of select="logtalk_entity/entity/type" /><xsl:text>**</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:text>``</xsl:text><xsl:value-of select="logtalk_entity/entity/name" /><xsl:text>``</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:call-template name="adornment">
		<xsl:with-param name="char" select="'='"/>
		<xsl:with-param name="n" select="4 + string-length(logtalk_entity/entity/name)"/>
	</xsl:call-template>
	<xsl:value-of select="$nl2" />
	<xsl:if test="logtalk_entity/entity/parameters">
		<xsl:for-each select="logtalk_entity/entity/parameters/parameter">
			<xsl:text>* ``</xsl:text><xsl:value-of select="name" /><xsl:text>`` - </xsl:text><xsl:value-of select="description" />
			<xsl:value-of select="$nl" />
		</xsl:for-each>
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:if test="logtalk_entity/entity/comment">
		<xsl:value-of select="logtalk_entity/entity/comment" />
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:apply-templates select="logtalk_entity/entity" />
	<xsl:apply-templates select="logtalk_entity/relations" />
	<xsl:apply-templates select="logtalk_entity/remarks" />
	<xsl:text>.. contents::</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:text>   :local:</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:text>   :backlinks: top</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:apply-templates select="logtalk_entity/predicates" />
	<xsl:apply-templates select="logtalk_entity/operators" />
	<xsl:apply-templates select="logtalk_entity/see_also" />
</xsl:template>


<xsl:template match="logtalk_entity/entity">
	<xsl:if test="author">
		<xsl:text>| **Author:** </xsl:text><xsl:value-of select="author" />
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="version">
		<xsl:text>| **Version:** </xsl:text><xsl:value-of select="version" />
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="date">
		<xsl:text>| **Date:** </xsl:text><xsl:value-of select="date" />
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:if test="copyright">
		<xsl:text>| **Copyright:** </xsl:text><xsl:value-of select="copyright" />
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="license">
		<xsl:text>| **License:** </xsl:text><xsl:value-of select="license" />
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:text>| **Compilation flags:**</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:text>|    ``</xsl:text><xsl:value-of select="compilation" /><xsl:text>``</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:if test="info">
		<xsl:for-each select="info">
			<xsl:text>| **</xsl:text><xsl:value-of select="key" /><xsl:text>:** </xsl:text><xsl:value-of select="value" />
			<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations">
	<xsl:choose>
		<xsl:when test="*">
			<xsl:if test="implements">
				<xsl:text>| **Implements:**</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="implements" />
			</xsl:if>
			<xsl:if test="imports">
				<xsl:text>| **Imports:**</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="imports" />
			</xsl:if>
			<xsl:if test="extends">
				<xsl:text>| **Extends:**</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="extends" />
			</xsl:if>
			<xsl:if test="instantiates">
				<xsl:text>| **Instantiates:**</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="instantiates" />
			</xsl:if>
			<xsl:if test="specializes">
				<xsl:text>| **Specializes:**</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="specializes" />
			</xsl:if>
			<xsl:if test="complements">
				<xsl:text>| **Complements:**</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="complements" />
			</xsl:if>
			<xsl:if test="provides">
				<xsl:text>| **Provides:**</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="provides" />
			</xsl:if>
			<xsl:if test="uses">
				<xsl:text>| **Uses:**</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="uses" />
			</xsl:if>
			<xsl:if test="alias">
				<xsl:text>| **Aliases:**</xsl:text><xsl:value-of select="$nl" />
				<xsl:apply-templates select="alias" />
			</xsl:if>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>| **Dependencies:**</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>|   (none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/provides" priority="1">
	<xsl:text>|    :ref:`</xsl:text>
	<xsl:call-template name="replace-string">
		<xsl:with-param name="text" select="to" />
		<xsl:with-param name="replace" select="'&#60;'" />
		<xsl:with-param name="with" select="'\&#60;'" />
	</xsl:call-template>
	<xsl:text>::</xsl:text><xsl:value-of select="resource" />
	<xsl:text> &lt;</xsl:text><xsl:value-of select="functor" /><xsl:text>&gt;`</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/uses" priority="1">
	<xsl:text>|    :ref:`</xsl:text>
	<xsl:call-template name="replace-string">
		<xsl:with-param name="text" select="name" />
		<xsl:with-param name="replace" select="'&#60;'" />
		<xsl:with-param name="with" select="'\&#60;'" />
	</xsl:call-template>
	<xsl:text> &lt;</xsl:text><xsl:value-of select="functor" /><xsl:text>&gt;`</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/alias" priority="1">
	<xsl:text>|    ``</xsl:text><xsl:value-of select="name" /><xsl:text>`` ``</xsl:text><xsl:value-of select="original" /><xsl:text>`` as ``</xsl:text><xsl:value-of select="alternative" /><xsl:text>``</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/complements" priority="1">
	<xsl:text>|    :ref:`</xsl:text>
	<xsl:call-template name="replace-string">
		<xsl:with-param name="text" select="name" />
		<xsl:with-param name="replace" select="'&#60;'" />
		<xsl:with-param name="with" select="'\&#60;'" />
	</xsl:call-template>
	<xsl:text> &lt;</xsl:text><xsl:value-of select="functor" /><xsl:text>&gt;`</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/relations/*" priority="0">
	<xsl:text>|    ``</xsl:text><xsl:value-of select="scope" /><xsl:text>`` :ref:`</xsl:text>
	<xsl:call-template name="replace-string">
		<xsl:with-param name="text" select="name" />
		<xsl:with-param name="replace" select="'&#60;'" />
		<xsl:with-param name="with" select="'\&#60;'" />
	</xsl:call-template>
	<xsl:text> &lt;</xsl:text><xsl:value-of select="functor" /><xsl:text>&gt;`</xsl:text>
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/predicates">
	<xsl:text>.. raw:: html</xsl:text>
   	<xsl:value-of select="$nl2" />
	<xsl:text>   &lt;/div&gt;</xsl:text>
   	<xsl:value-of select="$nl2" />
	<xsl:text>Inherited public predicates</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:text>---------------------------</xsl:text>
   	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="inherited/inherited_predicate">
			<xsl:apply-templates select="inherited/inherited_predicate" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
		</xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="$nl2" />
	<xsl:text>.. raw:: html</xsl:text>
   	<xsl:value-of select="$nl2" />
	<xsl:text>   &lt;/div&gt;</xsl:text>
   	<xsl:value-of select="$nl2" />
	<xsl:text>Public predicates</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:text>-----------------</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
			<xsl:text>(see entity ancestors)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:text>Protected predicates</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:text>--------------------</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
			<xsl:text>(see entity ancestors)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
	<xsl:text>Private predicates</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:text>------------------</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate" />
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
			<xsl:text>(see entity ancestors)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="*/inherited_predicate">
	<xsl:text>:ref:`</xsl:text>
	<xsl:value-of select="functor" />
	<xsl:text>::</xsl:text>
	<xsl:value-of select="name" />
	<xsl:text>`  </xsl:text>
</xsl:template>


<xsl:template match="*/predicate">
	<xsl:text>.. raw:: html</xsl:text>
   	<xsl:value-of select="$nl2" />
	<xsl:text>   &lt;div id=&quot;</xsl:text><xsl:value-of select="name" /><xsl:text>&quot;&gt; &lt;/div&gt;</xsl:text>
   	<xsl:value-of select="$nl2" />
	<xsl:text>.. index:: </xsl:text><xsl:value-of select="name" />
	<xsl:value-of select="$nl" />
	<xsl:text>.. _</xsl:text><xsl:value-of select="/logtalk_entity/entity/functor" />::<xsl:value-of select="name" /><xsl:text>:</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:text>``</xsl:text><xsl:value-of select="name" /><xsl:text>``</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:call-template name="adornment">
		<xsl:with-param name="char" select="'^'"/>
		<xsl:with-param name="n" select="4 + string-length(name)"/>
	</xsl:call-template>
	<xsl:value-of select="$nl2" />
	<xsl:if test="comment">
		<xsl:value-of select="comment" />
		<xsl:value-of select="$nl2" />
	</xsl:if>
	<xsl:text>| **Compilation flags:**</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:text>|    ``</xsl:text><xsl:value-of select="compilation" /><xsl:text>``</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:if test="template">
		<xsl:text>| **Template:**</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>|    ``</xsl:text><xsl:value-of select="template" /><xsl:text>``</xsl:text>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="arguments">
		<xsl:for-each select="arguments/argument">
			<xsl:text>|        ``</xsl:text><xsl:value-of select="name" /><xsl:text>`` - </xsl:text><xsl:value-of select="description" />
			<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:if test="meta">
		<xsl:text>| **Meta-predicate template:**</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>|    ``</xsl:text><xsl:value-of select="meta" /><xsl:text>``</xsl:text>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="coinductive">
		<xsl:text>| **Coinduction template:**</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:text>|    ``</xsl:text><xsl:value-of select="coinductive" /><xsl:text>``</xsl:text>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="mode">
		<xsl:text>| **Mode and number of proofs:**</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:for-each select="mode">
			<xsl:text>|    ``</xsl:text><xsl:value-of select="template" /><xsl:text>`` - ``</xsl:text><xsl:value-of select="proofs" /><xsl:text>``</xsl:text>
			<xsl:value-of select="$nl" />
		</xsl:for-each>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="exceptions">
		<xsl:text>| **Exceptions:**</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:for-each select="exceptions/exception">
			<xsl:text>|    </xsl:text><xsl:value-of select="condition" /><xsl:text>:</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>|        ``</xsl:text><xsl:value-of select="term" /><xsl:text>``</xsl:text>
			<xsl:value-of select="$nl" />
		</xsl:for-each>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="remarks">
		<xsl:text>| **Remarks:**</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:for-each select="remarks/remark">
			<xsl:text>|    </xsl:text><xsl:value-of select="topic" /><xsl:text>: </xsl:text><xsl:value-of select="text" />
			<xsl:value-of select="$nl" />
		</xsl:for-each>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="examples">
		<xsl:text>| **Examples:**</xsl:text>
		<xsl:value-of select="$nl" />
		<xsl:for-each select="examples/example">
			<xsl:text>|    </xsl:text><xsl:value-of select="description" />
			<xsl:value-of select="$nl" />
				<xsl:text>|      ``</xsl:text><xsl:value-of select="call" /><xsl:text>``</xsl:text>
				<xsl:value-of select="$nl" />
				<xsl:text>|      ``</xsl:text><xsl:value-of select="bindings" /><xsl:text>``</xsl:text>
				<xsl:value-of select="$nl" />
		</xsl:for-each>
		<xsl:value-of select="$nl" />
	</xsl:if>
	<xsl:if test="info">
		<xsl:for-each select="info">
			<xsl:text>| **</xsl:text><xsl:value-of select="key" /><xsl:text>:**</xsl:text>
			<xsl:value-of select="$nl" />
			<xsl:text>|    </xsl:text><xsl:value-of select="value" />
			<xsl:value-of select="$nl" />
		</xsl:for-each>
	</xsl:if>
	<xsl:value-of select="$nl" />
	<xsl:text>------------</xsl:text>
   	<xsl:value-of select="$nl2" />
</xsl:template>


<xsl:template match="logtalk_entity/operators">
	<xsl:text>Operators</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:text>---------</xsl:text>
	<xsl:value-of select="$nl2" />
	<xsl:choose>
		<xsl:when test="operator">
			<xsl:for-each select="operator">
				<xsl:text>``</xsl:text><xsl:value-of select="term" /><xsl:text>``</xsl:text>
				<xsl:value-of select="$nl" />
				<xsl:call-template name="adornment">
					<xsl:with-param name="char" select="'^'"/>
					<xsl:with-param name="n" select="4 + string-length(term)"/>
				</xsl:call-template>
				<xsl:value-of select="$nl2" />
				<xsl:text>| **Scope:**</xsl:text>
				<xsl:value-of select="$nl" />
				<xsl:text>|    ``</xsl:text><xsl:value-of select="scope" /><xsl:text>``</xsl:text>
				<xsl:value-of select="$nl2" />
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>(none)</xsl:text>
		</xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="$nl2" />
</xsl:template>


<xsl:template match="logtalk_entity/remarks">
	<xsl:text>| **Remarks:**</xsl:text>
	<xsl:value-of select="$nl" />
	<xsl:choose>
		<xsl:when test="remark">
			<xsl:value-of select="$nl" />
			<xsl:apply-templates select="remark" />
			<xsl:value-of select="$nl" />
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>|    (none)</xsl:text>
			<xsl:value-of select="$nl2" />
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="logtalk_entity/remarks/remark">
	<xsl:value-of select="$nl" />
	<xsl:text>   - *</xsl:text><xsl:value-of select="topic" /><xsl:text>:* </xsl:text><xsl:value-of select="text" />
	<xsl:value-of select="$nl" />
</xsl:template>


<xsl:template match="logtalk_entity/see_also">
	<xsl:if test="reference">
		<xsl:text>.. seealso::</xsl:text>
		<xsl:value-of select="$nl2" />
		<xsl:text>   </xsl:text>
		<xsl:for-each select="reference">
			<xsl:text>:ref:`</xsl:text>
			<xsl:call-template name="replace-string">
				<xsl:with-param name="text" select="name" />
				<xsl:with-param name="replace" select="'&#60;'" />
				<xsl:with-param name="with" select="'\&#60;'" />
			</xsl:call-template>
			<xsl:text> &lt;</xsl:text><xsl:value-of select="functor" /><xsl:text>&gt;`</xsl:text>
			<xsl:if test="position() != last()">
				<xsl:text>, </xsl:text>
			</xsl:if>
		</xsl:for-each>
		<xsl:value-of select="$nl2" />
	</xsl:if>
</xsl:template>


<xsl:template name="replace-string">
	<xsl:param name="text"/>
	<xsl:param name="replace"/>
	<xsl:param name="with"/>
	<xsl:choose>
		<xsl:when test="contains($text,$replace)">
			<xsl:value-of select="substring-before($text,$replace)"/>
			<xsl:value-of select="$with"/>
			<xsl:call-template name="replace-string">
				<xsl:with-param name="text" select="substring-after($text,$replace)"/>
				<xsl:with-param name="replace" select="$replace"/>
				<xsl:with-param name="with" select="$with"/>
			</xsl:call-template>
		</xsl:when>
		<xsl:otherwise>
			<xsl:value-of select="$text"/>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


</xsl:stylesheet>
