<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
              xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="menu">
    -<xsl:value-of select="(*/dish)[6]"/>
  </xsl:template>
</xsl:transform>
