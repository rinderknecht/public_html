<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="book" as="xs:integer">
    <xsl:call-template name="length">
      <xsl:with-param name="elm" select="author,title,contents/chapter"
                                 as="element()*"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="length" as="xs:integer">
    <xsl:param name="elm" as="element()*"/>
    <xsl:param name="len" as="xs:integer" select="0"/>
    <xsl:choose>
      <xsl:when test="empty($elm)">
        <xsl:sequence select="$len"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="length">
          <xsl:with-param name="elm" select="$elm[position()>1]"
                                     as="element()*"/>
          <xsl:with-param name="len" select="1 + $len" as="xs:integer"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:transform>
