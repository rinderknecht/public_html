<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="xml" version="1.0" encoding="UTF-8"
              indent="yes"/>

  <xsl:template match="/">
    <xsl:apply-templates select="numbers"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="numbers" as="element(numbers)">
    <xsl:copy>
      <xsl:call-template name="squeeze">
        <xsl:with-param name="nseq" select="num"/>
      </xsl:call-template>
    </xsl:copy>
  </xsl:template>

  <xsl:template name="squeeze" as="element(num)*">
    <xsl:param name="nseq" as="element(num)*"/>
    <xsl:choose>
      <xsl:when test="empty($nseq[position()>1])">
        <xsl:sequence select="$nseq"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$nseq[1]/@val ne $nseq[2]/@val">
          <xsl:sequence select="$nseq[1]"/>
        </xsl:if>
        <xsl:call-template name="squeeze">
          <xsl:with-param name="nseq" select="$nseq[position()>1]"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:transform>

