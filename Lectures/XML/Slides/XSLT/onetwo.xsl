<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
              xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="cars">
    <xsl:copy>
      <xsl:copy-of select="car[1]|car[2]"/>
    </xsl:copy>
  </xsl:template>
</xsl:transform>
