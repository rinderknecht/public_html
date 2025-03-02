<?xml version="1.0"?>
<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/">
    Today's menu:
    - <xsl:value-of select="/menu/entrees/dish[3]"/>
    - <xsl:value-of select="/menu/main/dish[1]"/>
    - <xsl:value-of select="/menu/desserts/dish[2]"/>
  </xsl:template>
</xsl:transform>
