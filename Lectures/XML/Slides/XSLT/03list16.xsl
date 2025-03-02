<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
              xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="menu">
     Dessert of the Day:
     <xsl:value-of select="*[3]/dish[2]" />
     Price: <xsl:value-of select="*[3]/dish[2]/@*[2]" />
  </xsl:template>
</xsl:transform>
