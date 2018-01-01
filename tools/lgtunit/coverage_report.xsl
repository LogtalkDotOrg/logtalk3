<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns="http://www.w3.org/1999/xhtml">

<!--
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  XSLT stylesheet for viewing code coverage report XML files in a browser
%  Last updated on April 7, 2017
%
%  This file is part of Logtalk <http://logtalk.org/>  
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
	encoding="utf-8"
	omit-xml-declaration="no"
	standalone="no"
	doctype-public="-//W3C//DTD XHTML 1.1//EN"
	doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"/>

<xsl:template match="/">
	<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
	<head>
		<meta http-equiv="content-type" content="text/html; charset=utf-8" />
		<title><xsl:value-of select="cover/testsuite" /></title>
		<style type="text/css">
			.percentage_bar {background-color: #ff4500; position: relative; font-size: small; width: 100%}
			.percentage_bar div {height: 20px; line-height: 20px;}
			.percentage_bar .percentage {position: absolute; background-color: #32cd32; left: 0px top: 0px; z-index: 0;}
			.percentage_bar .caption    {position: relative; text-align: center; color: #000; z-index: 1;}
			h1 {background-color: #f5f5f5; padding: 8px;}
			h2 {background-color: #f5f5f5; padding: 6px;}
			h3 {background-color: #f5f5f5; padding: 4px;}
			h4 {padding: 2px;}
			table {border: 1px solid black;}
			th {border: 1px solid black;}
			td {border: 1px solid black; padding: 3px;}
		</style>
	</head>
	<body>
		<h1>Logtalk Code Coverage Report</h1>
		<p>
			<strong>Test suite file: </strong> <code><xsl:value-of select="cover/testsuite" /></code><br />
			<strong>Test suite object: </strong> <code><xsl:value-of select="cover/object" /></code>
		</p>
		<p>
			<strong>Generated on: </strong> <code><xsl:value-of select="cover/timestamp" /></code>
		</p>

		<table style="width:50%;">
		    <tr>
		      <th style="width:30%;">Global Coverage</th>
		      <th style="width:10%;">Covered/Total</th> 
		      <th style="width:10%;">Percentage</th> 
		    </tr>
			<tr>
				<td style="width:30%;">Entities</td>
				<td style="width:10%; text-align:center">
					<div class="percentage_bar">
						<div class="percentage" style="width:{cover/entities_percentage}%">&#160;</div>
						<div class="caption"><xsl:value-of select="cover/entities_covered" />/<xsl:value-of select="cover/entities_total" /></div>
					</div>
				</td>
				<td style="width:10%; text-align:center"><xsl:value-of select="format-number(cover/entities_percentage, '##0.0')" /></td>
			</tr>
			<tr>
				<td style="width:30%;">Predicate Clauses and Grammar Rules</td>
				<td style="width:10%; text-align:center">
					<div class="percentage_bar">
						<div class="percentage" style="width:{cover/clauses_percentage}%">&#160;</div>
						<div class="caption"><xsl:value-of select="cover/clauses_covered" />/<xsl:value-of select="cover/clauses_total" /></div>
					</div>
				</td>
				<td style="width:10%; text-align:center"><xsl:value-of select="format-number(cover/clauses_percentage, '##0.0')" /></td>
			</tr>
		</table>

		<h2>Covered Entities</h2>
		<xsl:apply-templates select="cover/entities" />

	</body>
	</html>
</xsl:template>

<xsl:template match="cover/entities">
	<xsl:choose>
		<xsl:when test="entity">
			<xsl:for-each select="entity">
				<h3><code><xsl:value-of select="name" /></code></h3>
				<table style="width:50%; margin-bottom: 30px;">
				    <tr>
				      <th style="width:30%;">Entity Coverage</th>
				      <th style="width:10%;">Covered/Total</th> 
				      <th style="width:10%;">Percentage</th> 
				    </tr>
					<tr>
						<td style="width:30%;">Predicate Clauses and Grammar Rules</td>
						<td style="width:10%; text-align:center">
							<div class="percentage_bar">
								<div class="percentage" style="width:{percentage}%">&#160;</div>
								<div class="caption"><xsl:value-of select="covered" />/<xsl:value-of select="total" /></div>
							</div>
						</td>
						<td style="width:10%; text-align:center"><xsl:value-of select="format-number(percentage, '##0.0')" /></td>
					</tr>
				</table>
				<xsl:apply-templates select="predicates" />
			</xsl:for-each>
		</xsl:when>
		<xsl:otherwise>
			<h4>(no code coverage information collected)</h4>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="*/predicates">
	<table style="width:100%;">
	    <tr>
	      <th style="width:30%;">Predicate/Non-terminal Coverage</th>
	      <th style="width:15%;">Covered/Total</th> 
	      <th style="width:55%;">Covered Clause/Rule Indexes</th>
	    </tr>
		<xsl:for-each select="predicate">
			<tr>
				<td style="width:30%;"><code><xsl:value-of select="name" /></code></td>
				<td style="width:15%; text-align:center">
					<span>
						<div class="percentage_bar">
							<div class="percentage" style="width:{percentage}%">&#160;</div>
							<div class="caption"><xsl:value-of select="covered" />/<xsl:value-of select="total" /></div>
						</div>
					</span>
				</td>
				<td style="width:55%;"><xsl:value-of select="clauses" /></td>
			</tr>
		</xsl:for-each>
	</table>
</xsl:template>

</xsl:stylesheet>
