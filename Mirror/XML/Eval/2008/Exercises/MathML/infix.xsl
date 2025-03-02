<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="text" encoding="UTF-8"/>

  <xsl:template match="/" as="text()*">
    <xsl:apply-templates select="math/element()"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="ci|cn" as="text()">
    <xsl:sequence select="text()"/>
  </xsl:template>

  <xsl:template match="apply" as="text()*">
    <xsl:apply-templates select="element()[1]">
      <xsl:with-param name="arg" select="element()[position()>1]"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="eq" as="text()*">
    <xsl:param name="arg" as="element()*"/>
    <xsl:apply-templates select="$arg[1]"/>
    <xsl:text> = </xsl:text>
    <xsl:apply-templates select="$arg[2]"/>
  </xsl:template>

  <xsl:template name="n-ary" as="text()*">
    <xsl:param name="op" as="xs:string"/>
    <xsl:param name="arg" as="element()*"/>
    <xsl:if test="not(empty($arg))">
      <xsl:value-of select="$op"/>
      <xsl:apply-templates select="$arg[1]"/>
      <xsl:call-template name="n-ary">
        <xsl:with-param name="op"  select="$op"/>
        <xsl:with-param name="arg" select="$arg[position()>1]"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="plus" as="text()*">
    <xsl:param name="arg" as="element()*"/>
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="$arg[1]"/>
    <xsl:call-template name="n-ary">
      <xsl:with-param name="op"  select="' + '"/>
      <xsl:with-param name="arg" select="$arg[position()>1]"/>
    </xsl:call-template>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="minus" as="text()*">
    <xsl:param name="arg" as="element()*"/>
    <xsl:if test="empty($arg[position()>1])">
      <xsl:text>(-</xsl:text>
    </xsl:if>
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="$arg[1]"/>
    <xsl:call-template name="n-ary">
      <xsl:with-param name="op"   select="' - '"/>
      <xsl:with-param name="arg" select="$arg[position()>1]"/>
    </xsl:call-template>
    <xsl:text>)</xsl:text>
    <xsl:if test="empty($arg[position()>1])">
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="times" as="text()*">
    <xsl:param name="arg" as="element()*"/>
    <xsl:apply-templates select="$arg[1]"/>
    <xsl:call-template name="n-ary">
      <xsl:with-param name="op"  select="'*'"/>
      <xsl:with-param name="arg" select="$arg[position()>1]"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="power" as="text()*">
    <xsl:param name="arg" as="element()*"/>
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="$arg[1]"/>
    <xsl:text>)^(</xsl:text>
    <xsl:apply-templates select="$arg[2]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

</xsl:transform>
