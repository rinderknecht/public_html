<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="2.0" 
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="xml" version="1.0" indent="yes"/>

<!-- Matching the document root and running two transforms -->
<xsl:template match="book">
  <xsl:copy>
    <xsl:copy-of select="title|author"/>
    <toc>
      <xsl:apply-templates select="intro|chapter" mode="toc"/>
    </toc>
    <xsl:apply-templates select="intro|chapter"/>
  </xsl:copy>
</xsl:template>

<!-- Extracting the table of contents -->
<xsl:template match="intro|chapter|section" mode="toc">
  <xsl:copy>
    <xsl:value-of select="@title"/>
    <xsl:apply-templates select="section" mode="toc"/>
  </xsl:copy>
</xsl:template>

<!-- Elements are invariant -->
<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates select="@*" />
    <xsl:apply-templates />
  </xsl:copy>
</xsl:template>

<!-- Attributes become elements -->
<xsl:template match="@*">
  <xsl:element name="{name()}">
    <xsl:value-of select="."/>
  </xsl:element>
</xsl:template>

</xsl:stylesheet>
