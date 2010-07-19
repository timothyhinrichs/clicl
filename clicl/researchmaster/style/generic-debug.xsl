<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:import href="debug.xsl"/>
<xsl:template match="match">
<html>
<head>
	<title>Match</title>
	<xsl:call-template name="header-information">
		<xsl:with-param name="matchid" select="match-id"/>
	</xsl:call-template>
	<style type="text/css" media="all">@import 
		"/style/generic.css";</style>
		</head>
<body>
<xsl:call-template name="body-top"/>
<xsl:apply-templates select="state"/>

<xsl:call-template name="form"/>
<xsl:apply-templates select="history"/>
 <div id="traceswrap">
	<xsl:apply-templates select="tracewrap" />
 </div>
</body>
</html>
</xsl:template>

<xsl:template match="state">
	<div id="state">
	Current State
	<table border="0">
	<xsl:for-each select="fact">
		<tr>
			<td>
			<xsl:choose>
			<xsl:when test="prop-f">
			
			(<xsl:value-of select="prop-f"/>
			<xsl:for-each select="arg">
				&#160;<xsl:value-of select="."/>
			</xsl:for-each>)
			</xsl:when>
			<xsl:when test="prop">
			<xsl:value-of select="prop" />
			</xsl:when>
			</xsl:choose>
			</td>
		</tr>
	</xsl:for-each>
	</table>
	</div>
</xsl:template>

</xsl:stylesheet>