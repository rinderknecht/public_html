<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/" as="text()*">
    <xsl:apply-templates select="math/element()"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="csv" as="text()*">
    <xsl:param name="elm" as="element()*"/>
    <xsl:apply-templates select="$elm[1]"/>
    <xsl:if test="not(empty($elm[position()>1]))">
      <xsl:text>,</xsl:text>
      <xsl:call-template name="csv">
        <xsl:with-param name="elm" select="$elm[position()>1]"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="apply" as="text()*">
    <xsl:value-of select="name(element()[1])"/>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="csv">
      <xsl:with-param name="elm" select="element()[position()>1]"/>
    </xsl:call-template>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="ci|cn" as="text()">
    <xsl:sequence select="text()"/>
  </xsl:template>

</xsl:transform>
