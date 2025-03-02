<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
              xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="/">
    <xsl:text>INDEX</xsl:text>
    <xsl:apply-templates mode="index"/>
    <xsl:text>INFO</xsl:text>
      <xsl:apply-templates mode="info"/>
  </xsl:template>
  <xsl:template match="car" mode="index">
    <xsl:value-of select="@model" />
  </xsl:template>
  <xsl:template match="car" mode="info">
    <xsl:text>Model: </xsl:text>
    <xsl:value-of select="@model" />
    <xsl:text>, Manufacturer: </xsl:text>
    <xsl:value-of select="@manufacturer"/>
  </xsl:template>
</xsl:transform>
