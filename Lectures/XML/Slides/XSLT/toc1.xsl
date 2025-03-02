<?xml version="1.0" encoding="iso-8859-1"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="xhtml"
              doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
			  doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
			  indent="yes"/>

  <xsl:template match="book">
	<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
      <head>
		<title><xsl:value-of select="title"/></title>
	  </head>
	  <body>
		<h2><xsl:value-of select="title"/></h2>
		<p>by <xsl:value-of select="author/first"/>
  		   <xsl:text> </xsl:text>
		   <xsl:value-of select="author/last"/>
		</p>
	    <h3>Table of contents</h3>
		<ul>
		  <xsl:apply-templates select="chapter">
			<xsl:with-param name="depth" select="1"/>
		  </xsl:apply-templates>
		</ul>
	  </body>
	</html>
  </xsl:template>

  <xsl:template match="section|chapter">
	<xsl:param name="depth"/>
	<li xmlns="http://www.w3.org/1999/xhtml">
	  <xsl:value-of select="concat('[',$depth,'] ',title)"/>
	  <xsl:if test="not(empty(section))">
		<ul>
		  <xsl:apply-templates select="section">
			<xsl:with-param name="depth" select="$depth + 1"/>
		  </xsl:apply-templates>
		</ul>
	  </xsl:if>
	</li>
  </xsl:template>

</xsl:transform>
