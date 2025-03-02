<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="cars">
    <cars>
      <xsl:apply-templates/>
    </cars>
  </xsl:template>

  <xsl:template match="car">
    <car>
      <xsl:apply-templates select="attribute()"/>
    </car>
  </xsl:template>

  <xsl:template match="@model">
    <model>
      <xsl:value-of select="."/>
    </model>
  </xsl:template>

  <xsl:template match="@manufacturer">
    <manufacturer>
      <xsl:value-of select="." />
    </manufacturer>
  </xsl:template>
</xsl:transform>
