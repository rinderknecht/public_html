<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
              xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes"/>
  <xsl:template match="node()|attribute()">
    <xsl:copy>
      <xsl:apply-templates select="node()|attribute()"/>
    </xsl:copy>
  </xsl:template>
</xsl:transform>
