<?xml version="1.0" encoding="iso-8859-1"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="xhtml"
              doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
			  doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
			  indent="yes"/>

  <xsl:template match="cookbook">
	<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
      <head>
		<title><xsl:value-of select="title"/></title>
	  </head>
	  <body>
		<h2><xsl:value-of select="title"/></h2>
		<p><em>by<xsl:value-of select="author"/></em></p>
	    <h3>Table of contents</h3>
		<ol>
		  <xsl:apply-templates select="chapter"/>
		</ol>
	  </body>
	</html>
  </xsl:template>

  <xsl:template match="chapter">
	<li xmlns="http://www.w3.org/1999/xhtml">
	  <xsl:value-of select="text()"/>
	</li>
  </xsl:template>

</xsl:transform>
