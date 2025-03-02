<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="car">
    <xsl:call-template name="car"/>
  </xsl:template>
  <xsl:template name="car">
    Manufacturer: <xsl:value-of select="@manufacturer" />
    Model:        <xsl:value-of select="@model" />
  </xsl:template>
</xsl:transform>
