<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/" as="text()*">
    <!-- In the following, try select="math". -->
    <xsl:apply-templates select="math/element()"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="apply" as="text()*">
    <xsl:value-of select="name(element()[1])"/>
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="element()"/>
    <xsl:text>)</xsl:text>
    <xsl:if test="position() ne last()">,</xsl:if>
  </xsl:template>

  <xsl:template match="ci|cn" as="text()*">
    <xsl:sequence select="text()"/>
    <xsl:if test="position() ne last()">,</xsl:if>
  </xsl:template>

</xsl:transform>
