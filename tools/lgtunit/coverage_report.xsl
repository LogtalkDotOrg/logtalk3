<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns="http://www.w3.org/1999/xhtml">

<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  XSLT stylesheet for viewing lgtunit code coverage report files in a browser
%  Last updated on March 28, 2017
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
	method="xml"
	indent="yes"
	encoding="utf-8"
	omit-xml-declaration="no"
	standalone="no"
	doctype-public="-//W3C//DTD XHTML 1.1//EN"
	doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"/>


<xsl:template match="/">
	<xsl:processing-instruction name="xml-stylesheet">href="logtalk.css" type="text/css"</xsl:processing-instruction>

	<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
	<head>
		<meta http-equiv="content-type" content="text/html; charset=utf-8" />
		<title><xsl:value-of select="cover/testsuite" /></title>
		<style type="text/css">
			.percentage_bar {background-color: #ff4500; position: relative; font-size: small; width: 200px; margin: 1px;}
			.percentage_bar div {height: 20px; line-height: 20px;}
			.percentage_bar .percentage {position: absolute; background-color: #32cd32; left: 0px top: 0px; z-index: 0;}
			.percentage_bar .caption    {position: relative; text-align: center; color: #000; z-index: 1;}
		</style>
	</head>
	<body>
		<h1><xsl:value-of select="cover/testsuite" /></h1>
		<p><xsl:value-of select="cover/timestamp" /></p>
		<h4>Entity coverage</h4>
		<span>
			<xsl:value-of select="cover/entities_covered" />/<xsl:value-of select="cover/entities_total" />
			<div class="percentage_bar">
				<div class="percentage" style="width:{cover/entities_percentage}%">&#160;</div>
				<div class="caption"><xsl:value-of select="cover/entities_percentage" />%</div>
			</div>
		</span>
		<h4>Clause coverage</h4>
		<span>
			<xsl:value-of select="cover/clauses_covered" />/<xsl:value-of select="cover/clauses_total" />
			<div class="percentage_bar">
				<div class="percentage" style="width:{cover/clauses_percentage}%">&#160;</div>
				<div class="caption"><xsl:value-of select="cover/clauses_percentage" />%</div>
			</div>
		</span>
		<h2>Entities</h2>
		<xsl:apply-templates select="cover/entities" />
	</body>
	</html>
</xsl:template>

<xsl:template match="cover/entities">
	<xsl:for-each select="entity">
		<h3><xsl:value-of select="name" /></h3>
		<span>
			<xsl:value-of select="covered" />/<xsl:value-of select="total" />
			<div class="percentage_bar">
				<div class="percentage" style="width:{percentage}%">&#160;</div>
				<div class="caption"><xsl:value-of select="percentage" />%</div>
			</div>
		</span>
		<xsl:apply-templates select="predicates" />
	</xsl:for-each>
</xsl:template>

<xsl:template match="*/predicates">
	<table style="width:100%; border: 1px solid black;">
	    <tr>
	      <th style="width:30%; border: 1px solid black;">Predicate</th>
	      <th style="width:10%; border: 1px solid black;">Covered/Total</th> 
	      <th style="width:60%; border: 1px solid black;">Clauses</th>
	    </tr>
		<xsl:for-each select="predicate">
			<tr>
				<td style="width:30%; border: 1px solid black;"><xsl:value-of select="name" /></td>
				<td style="width:10%; border: 1px solid black; text-align:center">
					<span>
						<div class="percentage_bar">
							<div class="percentage" style="width:{percentage}%">&#160;</div>
							<div class="caption"><xsl:value-of select="covered" />/<xsl:value-of select="total" /></div>
						</div>
					</span>
				</td>
				<td style="width:60%; border: 1px solid black;"><xsl:value-of select="clauses" /></td>
			</tr>
		</xsl:for-each>
	</table>
</xsl:template>

</xsl:stylesheet>
