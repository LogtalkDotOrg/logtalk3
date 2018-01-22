<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:fo="http://www.w3.org/1999/XSL/Format">


<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  XSLT stylesheet for converting XML documenting files into PDF files
%  Last updated on March 14, 2016
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
	method="xml"
	indent="yes"
	encoding="utf-8"/>


<xsl:param name="format"/>

<xsl:variable name="height">
	<xsl:choose>
		<xsl:when test="$format='a4'">297mm</xsl:when>
		<xsl:when test="$format='us'">11.0in</xsl:when>
		<xsl:otherwise>297mm</xsl:otherwise>
	</xsl:choose>
</xsl:variable>

<xsl:variable name="width">
	<xsl:choose>
		<xsl:when test="$format='a4'">210mm</xsl:when>
		<xsl:when test="$format='us'">8.5in</xsl:when>
		<xsl:otherwise>210mm</xsl:otherwise>
	</xsl:choose>
</xsl:variable>


<xsl:template match ="/">

	<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">

		<fo:layout-master-set>
			<fo:simple-page-master
					master-name="simple"
					page-height="{$height}" 
					page-width="{$width}"
					margin-top="15mm" 
					margin-bottom="15mm" 
					margin-left="25mm" 
					margin-right="25mm">
				<fo:region-body margin-top="15mm" margin-bottom="15mm"/>
				<fo:region-before extent="15mm"/>
				<fo:region-after extent="15mm"/>
			</fo:simple-page-master>
		</fo:layout-master-set>

		<fo:page-sequence master-reference="simple">

			<fo:static-content flow-name="xsl-region-before">
				<fo:block>
					<fo:leader leader-pattern="rule" leader-length.optimum="100%"/>
				</fo:block>
				<fo:block
						text-align="end" 
						font-size="9pt" 
						font-family="sans-serif"
						font-weight="bold">
					<xsl:value-of select="logtalk_entity/entity/type"/>: <xsl:value-of select="logtalk_entity/entity/name"/>
				</fo:block>
			</fo:static-content> 

			<fo:static-content flow-name="xsl-region-after">
				<fo:block>
					<fo:leader leader-pattern="rule" leader-length.optimum="100%"/>
				</fo:block>
				<fo:block
						text-align="end" 
						font-size="9pt" 
						font-family="sans-serif"
						font-weight="bold">
					<fo:page-number/> of <fo:page-number-citation ref-id="end"/>
				</fo:block>
			</fo:static-content> 

			<fo:flow flow-name="xsl-region-body">
    	   		<fo:block
    	   				font-size="18pt" 
    	        	    font-family="sans-serif" 
    	        	    font-weight="bold"
    	        	    space-after="8pt">
    	    		<xsl:value-of select="logtalk_entity/entity/name"/>
    	    	</fo:block>
 				<xsl:apply-templates select="logtalk_entity/entity"/>
				<xsl:apply-templates select="logtalk_entity/relations"/>
				<xsl:apply-templates select="logtalk_entity/predicates"/>
				<xsl:apply-templates select="logtalk_entity/operators"/>
				<xsl:apply-templates select="logtalk_entity/remarks"/>
				<xsl:apply-templates select="logtalk_entity/see_also"/>
				<fo:block id="end"/>
			</fo:flow>

		</fo:page-sequence>

	</fo:root>

</xsl:template>


<xsl:template match="logtalk_entity/entity">

	<xsl:if test="comment">
		<fo:block
				margin-left="10mm"
				font-size="10pt"
				font-family="serif">
			<xsl:value-of select="comment"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="parameters">
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm"
				space-before="4pt">
		</fo:block>
		<xsl:for-each select="parameters/parameter">
			<fo:block
					margin-left="10mm">
				<fo:inline font-size="9pt" font-family="monospace"><xsl:value-of select="name"/></fo:inline>
				<fo:inline font-size="10pt" font-family="serif"> - <xsl:value-of select="description"/></fo:inline>
			</fo:block>
		</xsl:for-each>
	</xsl:if>

	<xsl:if test="author">
		<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always"
				space-before="8pt">
			author:
		</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="author"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="version">
		<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
			version:
		</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="version"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="date">
		<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
			date:
		</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="date"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="copyright">
		<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
			copyright:
		</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="copyright"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="license">
		<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always">
			license:
		</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="license"/>
		</fo:block>
	</xsl:if>

	<fo:block
			font-size="10pt"
			font-family="serif"
			space-before="8pt"
			keep-with-next="always">
		compilation flags:
	</fo:block>
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm" 
			space-after="4pt">
		<xsl:value-of select="compilation"/>
	</fo:block>

	<xsl:if test="info">
		<xsl:for-each select="info">
       		<fo:block
					font-size="10pt"
					font-family="serif" 
					keep-with-next="always">
     			<xsl:value-of select="key"/>:
     		</fo:block>
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="10mm">
				<xsl:value-of select="value"/>
			</fo:block>
		</xsl:for-each>
	</xsl:if>

</xsl:template>


<xsl:template match="logtalk_entity/relations">
	<fo:block
			font-size="10pt"
			font-family="serif"
			space-before="4pt">
	</fo:block>
	<xsl:choose>
		<xsl:when test="*">
			<xsl:if test="implements">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					implements:
				</fo:block>
				<xsl:apply-templates select="implements"/>
			</xsl:if>
			<xsl:if test="imports">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					imports:
				</fo:block>
				<xsl:apply-templates select="imports"/>
			</xsl:if>
			<xsl:if test="extends">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					extends:
				</fo:block>
				<xsl:apply-templates select="extends"/>
			</xsl:if>
			<xsl:if test="instantiates">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					instantiates:
				</fo:block>
				<xsl:apply-templates select="instantiates"/>
			</xsl:if>
			<xsl:if test="specializes">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					specializes:
				</fo:block>
				<xsl:apply-templates select="specializes"/>
			</xsl:if>
			<xsl:if test="provides">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					provides:
				</fo:block>
				<xsl:apply-templates select="provides"/>
			</xsl:if>
			<xsl:if test="uses">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					uses:
				</fo:block>
				<xsl:apply-templates select="uses"/>
			</xsl:if>
			<xsl:if test="calls">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					calls:
				</fo:block>
				<xsl:apply-templates select="calls"/>
			</xsl:if>
			<xsl:if test="alias">
				<fo:block
						font-size="10pt"
						font-family="serif" 
						keep-with-next="always">
					aliases:
				</fo:block>
				<xsl:apply-templates select="alias"/>
			</xsl:if>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="4pt">
				(no dependencies on other entities)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk_entity/relations/provides" priority="1">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="to"/>::<xsl:value-of select="resource" />
	</fo:block>
</xsl:template>


<xsl:template match="logtalk_entity/relations/uses" priority="1">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="name"/>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk_entity/relations/calls" priority="1">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="name"/>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk_entity/relations/alias" priority="1">
	<fo:block
			font-size="9pt"
			margin-left="10mm">
		<fo:inline font-family="monospace"><xsl:value-of select="name" /><xsl:text> </xsl:text><xsl:value-of select="original" /></fo:inline>
		<fo:inline font-family="serif" font-style="italic"> aka </fo:inline>
		<fo:inline font-family="monospace"><xsl:value-of select="alternative" /></fo:inline>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk_entity/relations/*" priority="0">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="scope"/><xsl:text> </xsl:text><xsl:value-of select="name"/>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk_entity/predicates">

	<fo:block
			font-size="14pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="18pt">
		Public interface
	</fo:block>
	<xsl:choose>
		<xsl:when test="public/predicate">
			<xsl:apply-templates select="public/predicate"/>
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(see related entities)
			</fo:block>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(none)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>

	<fo:block
			font-size="14pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="18pt">
		Protected interface
	</fo:block>
	<xsl:choose>
		<xsl:when test="protected/predicate">
			<xsl:apply-templates select="protected/predicate"/>
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(see related entities)
			</fo:block>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(none)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>

	<fo:block
			font-size="14pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="18pt">
		Private predicates
	</fo:block>
	<xsl:choose>
		<xsl:when test="private/predicate">
			<xsl:apply-templates select="private/predicate"/>
		</xsl:when>
		<xsl:when test="/logtalk_entity/relations/*">
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(see related entities)
			</fo:block>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(none)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>

</xsl:template>


<xsl:template match="*/predicate">

	<fo:block
			font-size="12pt" 
			font-family="monospace" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="10pt">
		<xsl:value-of select="name"/>
	</fo:block>

	<xsl:if test="comment">
		<fo:block
				margin-left="10mm"
				font-size="10pt" 
				font-family="serif" 
				space-before="4pt" 
				space-after="8pt">
			<xsl:value-of select="comment"/>
		</fo:block>
	</xsl:if>

	<fo:block
			font-size="10pt"
			font-family="serif"
			keep-with-next="always">
		compilation flags:
	</fo:block>
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="compilation"/>
	</fo:block>

	<xsl:if test="template">
      	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always"
				space-before="4pt">
     		template:
     	</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="template"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="arguments">
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm"
				space-before="4pt">
		</fo:block>
		<xsl:for-each select="arguments/argument">
			<fo:block
					margin-left="10mm">
				<fo:inline font-size="9pt" font-family="monospace"><xsl:value-of select="name"/></fo:inline>
				<fo:inline font-size="10pt" font-family="serif" font-style="italic"> - <xsl:value-of select="description"/></fo:inline>
			</fo:block>
		</xsl:for-each>
	</xsl:if>

	<xsl:if test="meta">
      	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always"
				space-before="4pt">
     		meta-predicate template:
     	</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="meta"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="coinductive">
      	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always"
				space-before="4pt">
     		coinductive predicate template:
     	</fo:block>
		<fo:block
				font-size="9pt"
				font-family="monospace"
				margin-left="10mm">
			<xsl:value-of select="coinductive"/>
		</fo:block>
	</xsl:if>

	<xsl:if test="mode">
       	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always"
				space-before="4pt">
     		mode - number of proofs:
     	</fo:block>
		<xsl:for-each select="mode">
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="10mm">
				<xsl:value-of select="template"/> - <xsl:value-of select="proofs"/>
			</fo:block>
		</xsl:for-each>
	</xsl:if>

	<xsl:if test="exceptions">
       	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always"
				space-before="4pt">
     		exceptions:
     	</fo:block>
		<xsl:for-each select="exceptions/exception">
       		<fo:block
					font-size="10pt"
					font-family="serif" 
					keep-with-next="always"
					margin-left="10mm">
     			<xsl:value-of select="condition" />:
     		</fo:block>
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="20mm">
				<xsl:value-of select="term" />
			</fo:block>
		</xsl:for-each>
	</xsl:if>

	<xsl:if test="remarks">
       	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always"
				space-before="4pt">
     		remarks:
     	</fo:block>
		<xsl:for-each select="remarks/remark">
       		<fo:block
					font-size="10pt"
					font-family="serif"
					margin-left="10mm">
     			<xsl:value-of select="topic" />: <xsl:value-of select="text" />
     		</fo:block>
		</xsl:for-each>
	</xsl:if>

	<xsl:if test="examples">
       	<fo:block
				font-size="10pt"
				font-family="serif" 
				keep-with-next="always"
				space-before="4pt">
     		examples:
     	</fo:block>
		<xsl:for-each select="examples/example">
       		<fo:block
					font-size="10pt"
					font-family="serif"
					font-style="italic"
					keep-with-next="always"
					margin-left="10mm">
     			<xsl:value-of select="description" />
     		</fo:block>
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="20mm">
				<xsl:value-of select="call" />
			</fo:block>
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="20mm" 
					space-after="4pt">
				<xsl:value-of select="bindings" />
			</fo:block>
		</xsl:for-each>
	</xsl:if>

	<xsl:if test="info">
		<xsl:for-each select="info">
       		<fo:block
					font-size="10pt"
					font-family="serif" 
					keep-with-next="always"
					space-before="4pt">
    	 			<xsl:value-of select="key"/>:
    	 		</fo:block>
			<fo:block
					font-size="9pt"
					font-family="monospace"
					margin-left="10mm">
				<xsl:value-of select="value"/>
			</fo:block>
		</xsl:for-each>
	</xsl:if>

</xsl:template>


<xsl:template match="logtalk_entity/operators">

	<fo:block
			font-size="14pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="18pt">
		Operators
	</fo:block>
	<xsl:choose>
		<xsl:when test="*">
			<xsl:for-each select="operator">
				<fo:block
					font-size="12pt" 
					font-family="monospace" 
					font-weight="bold" 
					keep-with-next="always"
					space-before="10pt">
					<xsl:value-of select="term"/> (<xsl:value-of select="scope"/>)
				</fo:block>
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(none)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>

</xsl:template>


<xsl:template match="logtalk_entity/remarks">

	<fo:block
			font-size="14pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="18pt">
		Remarks
	</fo:block>
	<xsl:choose>
		<xsl:when test="*">
			<xsl:for-each select="remark">
       			<fo:block
						font-size="10pt"
						font-family="serif"
						font-style="italic"
						keep-with-next="always"
						space-before="10pt">
     				<xsl:value-of select="topic" />
     			</fo:block>
				<fo:block
						font-size="10pt"
						font-family="serif"
						margin-left="10mm">
					<xsl:value-of select="text" />
				</fo:block>
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(none)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>

</xsl:template>


<xsl:template match="logtalk_entity/see_also">

	<fo:block
			font-size="14pt" 
			font-family="sans-serif" 
			font-weight="bold" 
			keep-with-next="always"
			space-before="18pt">
		See also
	</fo:block>
	<xsl:choose>
		<xsl:when test="*">
			<xsl:for-each select="reference">
				<fo:block
						font-size="9pt"
						font-family="monospace"
						margin-left="10mm">
					<xsl:value-of select="name" />
				</fo:block>
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<fo:block
					font-size="10pt" 
					font-family="serif" 
					font-style="italic"
					space-before="10pt">
				(none)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>

</xsl:template>


</xsl:stylesheet>
