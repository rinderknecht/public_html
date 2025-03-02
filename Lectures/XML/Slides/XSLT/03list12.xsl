<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
              xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <xsl:template match="menu">
     Dessert of the Day:
     <xsl:value-of select="desserts/dish[2]" />
     Price: <xsl:value-of select="desserts/dish[2]/@price" />
  </xsl:template>
</xsl:transform>
