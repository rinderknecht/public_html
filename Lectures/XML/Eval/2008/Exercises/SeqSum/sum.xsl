<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/">
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="numbers" as="xs:integer">
    <xsl:call-template name="sum">
      <xsl:with-param name="seq" select="num" as="element(num)*"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="sum" as="xs:integer">
    <xsl:param name="seq" as="element(num)*"/>
    <xsl:choose>
      <xsl:when test="empty($seq)">
        <xsl:sequence select="0"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="rem" as="xs:integer">
          <xsl:call-template name="sum">
            <xsl:with-param name="seq" select="$seq[position()>1]"
                                       as="element(num)*"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:sequence select="xs:integer($seq[1]/text()) + $rem"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:transform>
