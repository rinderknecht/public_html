<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="car"> <xsl:apply-templates select="*"/> </xsl:template>
  <xsl:template match="model">
    Model: <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="manufacturer">
    Manufacturer: <xsl:value-of select="."/>
  </xsl:template>
</xsl:transform>
