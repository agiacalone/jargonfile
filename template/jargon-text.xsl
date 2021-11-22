<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<!-- Render XML to text. -->

<xsl:import href="jargon.xsl"/>

<xsl:param name="suppress.glossary.titlepage" select="1"/>
<xsl:param name="suppress.glossdev.contents" select="1"/>

<!-- Surround links with {} as in older versions. -->
<xsl:template match="glossdef//glossterm">
    <xsl:text>{</xsl:text><xsl:call-template name="inline.charseq"/><xsl:text>}</xsl:text>
</xsl:template>

<!-- Surround entry keywords with :: -->
<xsl:template match="glossentry/glossterm">
    <xsl:text>:</xsl:text><xsl:apply-templates/><xsl:text>: </xsl:text>
    <!-- metadata goes right after glossary term -->
    <xsl:if test="../abbrev">
      <xsl:for-each select="../abbrev/*">
        <xsl:if test="position() &gt; 1">, </xsl:if>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:if>
</xsl:template>

</xsl:stylesheet>
