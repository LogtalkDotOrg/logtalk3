<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:fo="http://www.w3.org/1999/XSL/Format">


<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
% 
%   XSLT stylesheet for converting XML documenting files into PDF files
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
					<xsl:value-of select="logtalk/entity/type"/>: <xsl:value-of select="logtalk/entity/name"/>
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
    	    		<xsl:value-of select="logtalk/entity/name"/>
    	    	</fo:block>
 				<xsl:apply-templates select="logtalk/entity"/>
				<xsl:apply-templates select="logtalk/relations"/>
				<xsl:apply-templates select="logtalk/predicates"/>
				<xsl:apply-templates select="logtalk/operators"/>
				<xsl:apply-templates select="logtalk/remarks"/>
				<fo:block id="end"/>
			</fo:flow>

		</fo:page-sequence>

	</fo:root>

</xsl:template>


<xsl:template match="logtalk/entity">

	<xsl:if test="comment">
		<fo:block
				margin-left="10mm"
				font-size="10pt"
				font-family="serif" 
				font-style="italic">
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
					font-size="9pt"
					font-family="monospace"
					margin-left="10mm">
				<xsl:value-of select="name"/> - <xsl:value-of select="description"/>
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


<xsl:template match="logtalk/relations">
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
				(no dependencies on other files)
			</fo:block>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<xsl:template match="logtalk/relations/provides" priority="1">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="to"/>::<xsl:value-of select="resource" />
	</fo:block>
</xsl:template>


<xsl:template match="logtalk/relations/uses" priority="1">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="name"/>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk/relations/calls" priority="1">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="name"/>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk/relations/alias" priority="1">
	<fo:block
			font-size="9pt"
			margin-left="10mm">
		<fo:inline font-family="monospace"><xsl:value-of select="name" /><xsl:text> </xsl:text><xsl:value-of select="original" /></fo:inline>
		<fo:inline font-family="serif" font-style="italic"> aka </fo:inline>
		<fo:inline font-family="monospace"><xsl:value-of select="alternative" /></fo:inline>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk/relations/*" priority="0">
	<fo:block
			font-size="9pt"
			font-family="monospace"
			margin-left="10mm">
		<xsl:value-of select="scope"/><xsl:text> </xsl:text><xsl:value-of select="name"/>
	</fo:block>
</xsl:template>


<xsl:template match="logtalk/predicates">

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
		<xsl:when test="/logtalk/relations/*">
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
		<xsl:when test="/logtalk/relations/*">
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
		<xsl:when test="/logtalk/relations/*">
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
				font-style="italic"
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


<xsl:template match="logtalk/operators">

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


<xsl:template match="logtalk/remarks">

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


</xsl:stylesheet>
