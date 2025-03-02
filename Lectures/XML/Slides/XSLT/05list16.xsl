<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform version="2.0"
              xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes"/>
  <xsl:template match="cars">
    <table border="1" width="500">
      <xsl:apply-templates/>
    </table>
  </xsl:template>
  <xsl:template match="car">
    <tr bgcolor="#dddddd">
      <xsl:apply-templates select="attribute()"/>
    </tr>
  </xsl:template>
  <xsl:template match="attribute()">
    <td> <xsl:value-of select="."/> </td>
  </xsl:template>
</xsl:transform>
