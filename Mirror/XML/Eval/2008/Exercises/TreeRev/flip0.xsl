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
      <xsl:copy-of select="author"/>
      <xsl:call-template name="flip">
        <xsl:with-param name="sections" select="section"/>
      </xsl:call-template>
    </xsl:copy>
  </xsl:template>

  <xsl:template name="rev">
    <xsl:param name="items" as="item()*"/>
    <xsl:if test="not(empty($items))">
      <xsl:call-template name="rev">
        <xsl:with-param name="items" select="$items[position()>1]"/>
      </xsl:call-template>
      <xsl:sequence select="$items[1]"/>
    </xsl:if>
  </xsl:template>

  <xsl:template name="flip" as="element(section)*">
    <xsl:param name="sections" as="element(section)*"/>
    <xsl:call-template name="flip_rev">
      <xsl:with-param name="sections" as="element(section)*">
        <xsl:call-template name="rev">
          <xsl:with-param name="items" select="$sections"/>
        </xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="flip_rev" as="element(section)*">
    <xsl:param name="sections" as="element(section)*"/>
    <xsl:if test="not(empty($sections))">
      <section title="{$sections[1]/@title}">
        <xsl:call-template name="flip">
          <xsl:with-param name="sections" select="$sections[1]/section"/>
        </xsl:call-template>
      </section>
      <xsl:call-template name="flip_rev">
        <xsl:with-param name="sections" select="$sections[position()>1]"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

</xsl:transform>
