<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="text"/>
  <xsl:strip-space elements="*"/>

  <!-- The root element -->
  <xsl:template match="math">
    <xsl:apply-templates/> <!-- By default, select the children. -->
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Constants -->
  <xsl:template match="ci|cn">
    <xsl:sequence select="text()"/>
  </xsl:template>

  <!-- Handle applications and dispatch to the relevant template -->
  <xsl:template match="apply">
    <xsl:apply-templates select="element()[1]">
      <xsl:with-param name="args" select="element()[position()>1]"/>
    </xsl:apply-templates>
  </xsl:template>

  <!-- Prints the equality (=) operator and both sides -->
  <xsl:template match="eq">
    <xsl:param name="args" as="element()*"/>
    <!-- TO BE DONE -->
  </xsl:template>

  <!-- Prints operator $op and its arguments $args -->
  <xsl:template name="n-ary"> 
    <xsl:param name="op" as="xs:string"/>
    <xsl:param name="args" as="element()*"/>
    <!-- TO BE DONE -->
  </xsl:template>

  <!-- Handle addition (+) -->
  <xsl:template match="plus">
    <xsl:param name="args" as="element()*"/>
    <!-- TO BE DONE -->
  </xsl:template>

  <!-- Handle unary and binary subtraction (-) -->
  <xsl:template match="minus">
    <xsl:param name="args" as="element()*"/>
    <!-- TO BE DONE -->
  </xsl:template>

  <!-- Handle multiplication (*) -->
  <xsl:template match="times">
    <xsl:param name="args" as="element()*"/>
    <!-- TO BE DONE -->
  </xsl:template>

  <!-- Handle exponentiation (^) -->
  <xsl:template match="power">
    <xsl:param name="args" as="element()*"/>
    <!-- TO BE DONE -->
  </xsl:template>

</xsl:transform>
