<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0" 
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>

<xsl:template match="numbers">
  <xsl:apply-templates select="num"/>
</xsl:template>

<xsl:template match="num">
  <xsl:value-of select="text()"/>
  <xsl:choose>
    <xsl:when test="position() ne last()">
      <xsl:value-of select="', '"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="'.'"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:transform>
