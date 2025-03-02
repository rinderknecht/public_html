<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema"
               exclude-result-prefixes="xs">

  <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

  <xsl:template match="/">
    <xsl:apply-templates select="book"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="book" as="element(book)">
    <xsl:copy>
      <xsl:attribute name="title" select="@title"/>
      <xsl:copy-of select="author"/>
      <xsl:apply-templates select="chapter"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="chapter|section" as="element()*">
    <xsl:param name="siblings" as="element(section)*"/>
    <xsl:apply-templates select="$siblings[1]">
      <xsl:with-param name="siblings" select="$siblings[position()>1]"/>
    </xsl:apply-templates>
    <xsl:copy>
      <xsl:attribute name="title" select="@title"/>
      <xsl:apply-templates select="section[1]">
        <xsl:with-param name="siblings" select="section[position()>1]"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

</xsl:transform>
