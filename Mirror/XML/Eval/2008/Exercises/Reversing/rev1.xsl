<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema"
               exclude-result-prefixes="xs">

  <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

  <xsl:template match="/">
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="book" as="element(book)">
    <xsl:copy>
      <xsl:sequence select="author"/>
      <xsl:sequence select="title"/>
      <contents>
        <xsl:call-template name="rev">
          <xsl:with-param name="items" select="contents/chapter"
                                       as="element(chapter)*"/>
        </xsl:call-template>
      </contents>
    </xsl:copy>
  </xsl:template>

  <xsl:template name="rev" as="item()*">
    <xsl:param name="items" as="item()*"/>
    <xsl:if test="not(empty($items))">
      <xsl:call-template name="rev">
        <xsl:with-param name="items" select="$items[position()>1]"
                                     as="item()*"/>
      </xsl:call-template>
      <xsl:sequence select="$items[1]"/>
    </xsl:if>
  </xsl:template>

</xsl:transform>
