<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0" 
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
			   xmlns:my="file://functions.uri">
  <xsl:output method="text"/>

  <xsl:function name="my:names">
	<xsl:param name="nodes"/>
	<xsl:if test="not(empty($nodes))">
	  <xsl:value-of select="(name($nodes[1]),
							 my:names($nodes[position()>1]))"/>
	</xsl:if>
  </xsl:function>

  <xsl:template match="/element()">
	<xsl:value-of select="my:names(element())"/>
  </xsl:template>

</xsl:transform>
