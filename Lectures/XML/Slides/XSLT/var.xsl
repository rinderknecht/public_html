<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:transform version="2.0"
              xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="/">
    <xsl:variable name="v" select="0"/>
    <xsl:variable name="w" select="1 + $v"/>
    <xsl:value-of select="$w"/>
  </xsl:template>
</xsl:transform>
