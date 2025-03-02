<?xml version="1.0"?>
<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="entrees">
    Entrees:
    - <xsl:value-of select="dish"/>
  </xsl:template>
  <xsl:template match="main"/>
  <xsl:template match="desserts"/>
</xsl:transform>
