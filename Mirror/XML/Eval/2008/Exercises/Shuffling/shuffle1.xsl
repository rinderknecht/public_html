<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

  <xsl:template match="/">
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="persons" as="element(persons)">
    <xsl:copy>
      <xsl:call-template name="shuffle">
        <xsl:with-param name="left" select="names/name"/>
        <xsl:with-param name="right" select="notes/note"/>
      </xsl:call-template>
    </xsl:copy>
  </xsl:template>

  <xsl:template name="shuffle" as="item()*">
    <xsl:param name="left"  as="item()*"/>
    <xsl:param name="right" as="item()*"/>
    <xsl:if test="not(empty($left)) and not(empty($right))">
      <xsl:sequence select="$left[1]"/>
      <xsl:sequence select="$right[1]"/>
      <xsl:call-template name="shuffle">
        <xsl:with-param name="left"  select="$left[position()>1]"/>
        <xsl:with-param name="right" select="$right[position()>1]"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

</xsl:transform>
