<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<xsl:import href="docbook-xsl-1.61.0/xhtml/docbook.xsl"/>

<xsl:param name="navig.graphics" select="0"/>
<xsl:param name="navig.graphics.path">../graphics/</xsl:param>
<xsl:param name="navig.graphics.extension" select="'.png'"/>

<xsl:param name="make.valid.html" select="1"/>
<xsl:param name="html.cleanup" select="1"/>
<xsl:param name="shade.verbatim" select="1"/>

<xsl:param name="glossterm.auto.link" select="1"/>
<xsl:param name="generate.section.toc.level" select="1"/>
<xsl:param name="chunk.first.sections" select="1"/>
<xsl:param name="use.id.as.filename" select="1"/>

<xsl:param name="emphasis.propagates.style" select="1"/>
<xsl:param name="para.propagates.style" select="1"/>
<xsl:param name="phrase.propagates.style" select="1"/>

<xsl:param name="html.stylesheet">jargon.css</xsl:param>
<xsl:param name="css.decoration">1</xsl:param>

<xsl:param name="suppress.glossary.titlepage" select="0"/>
<xsl:param name="suppress.glossdev.contents" select="1"/>

<!-- suppress lists of figures, tables, examples, and equations -->
<xsl:param name="generate.toc">
appendix  toc,title
article/appendix  nop
article   toc,title
book      toc,title
chapter   toc,title
part      toc,title
preface   toc,title
qandadiv  toc
qandaset  toc
reference toc,title
sect1     toc
sect2     toc
sect3     toc
sect4     toc
sect5     toc
section   toc
set       toc,title
</xsl:param>

<!-- Check that the stylesheet release is new enough for this layer to work -->
<xsl:template match="book" mode="process.root">
  <!-- This is probably unnecessarily pedantic -->
  <xsl:variable name="version-major" select="substring-before($VERSION, '.')"/>
  <xsl:variable name="version-parts" select="substring-after($VERSION, '.')"/>
  <xsl:variable name="version-minor" select="substring-before($version-parts, '.')"/>
  <xsl:variable name="version" select="concat($version-major, '.', $version-minor)"/>
  <xsl:if test="$version &lt; 1.61">
    <xsl:message terminate="yes">
      <xsl:text>This stylesheet requires version 1.61 or higher of the base</xsl:text>
      <xsl:text>&#10;</xsl:text>
      <xsl:text>DocBook XSL Stylesheets (http://sf.net/projects/docbook)</xsl:text>
    </xsl:message>
  </xsl:if>
  <xsl:apply-imports/>
</xsl:template>

<!-- Unsuppress bibliography abstracts -->
<xsl:template match="abstract" mode="bibliography.mode">
  <xsl:apply-templates/>
</xsl:template>

<!-- embolden entry keywords -->
<xsl:template match="glossentry/glossterm">
  <dt id='{../@id}'>
    <b>
      <xsl:apply-templates/>
    </b>
    <!-- metadata goes right after glossary term -->
    <xsl:if test="../abbrev">
      <xsl:text>: </xsl:text>
      <xsl:for-each select="../abbrev/*">
        <xsl:if test="position() &gt; 1">, </xsl:if>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:if>
  </dt>
</xsl:template>

<!-- Support glossary as a chunk; add a simple ToC; write frames -->
<xsl:template match="glossary">
  <xsl:variable name="here" select="."/>

  <div class="{name(.)}">
    <xsl:if test="$generate.id.attributes != 0">
      <xsl:attribute name="id">
        <xsl:call-template name="object.id"/>
      </xsl:attribute>
    </xsl:if>

    <xsl:if test="$suppress.glossary.titlepage = 1">
      <xsl:call-template name="glossary.titlepage"/>
      <dl>
	<xsl:for-each select="glossdiv">
	  <dt>
	    <a>
	      <xsl:attribute name="href">
		<xsl:call-template name="href.target">
		  <xsl:with-param name="context" select="$here"/>
		</xsl:call-template>
	      </xsl:attribute>
	      <xsl:value-of select="title"/>
	    </a>
	  </dt>
	  <dd>
	    <dl>
	      <xsl:for-each select="glossentry">
		<dt>
		  <a>
		    <xsl:attribute name="href">
		      <xsl:call-template name="href.target">
			<xsl:with-param name="context" select="$here"/>
		      </xsl:call-template>
		    </xsl:attribute>
		    <xsl:value-of select="glossterm[1]"/>
		  </a>
		</dt>
	      </xsl:for-each>
	    </dl>
	  </dd>
	</xsl:for-each>
      </dl>
    </xsl:if>

    <xsl:choose>
      <xsl:when test="glossdiv">
        <xsl:apply-templates select="(glossdiv[1]/preceding-sibling::*)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="(glossentry[1]/preceding-sibling::*)"/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:choose>
      <xsl:when test="glossdiv">
        <xsl:apply-templates select="glossdiv"/>
      </xsl:when>
      <xsl:otherwise>
        <dl>
          <xsl:apply-templates select="glossentry"/>
        </dl>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:if test="not(parent::article)">
      <xsl:call-template name="process.footnotes"/>
    </xsl:if>
  </div>
</xsl:template>

<!-- Support glossdiv as a chunk; add a simple ToC to glossary divisions -->
<xsl:template match="glossdiv">
  <xsl:variable name="here" select="."/>

  <div class="{name(.)}">
    <xsl:apply-templates select="(glossentry[1]/preceding-sibling::*)"/>

    <xsl:if test="$suppress.glossdev.contents=1">
      <dl>
	<xsl:for-each select="glossentry">
	  <dt>
	    <a>
	      <xsl:attribute name="href">
		<xsl:call-template name="href.target">
		  <xsl:with-param name="context" select="$here"/>
		</xsl:call-template>
	      </xsl:attribute>
	      <xsl:value-of select="glossterm[1]"/>
	    </a>
	  </dt>
	</xsl:for-each>
      </dl>
    </xsl:if>

    <xsl:apply-templates select="glossentry"/>
  </div>
</xsl:template>

<!-- Override toc mode template for glossary to output links to both unframed -->
<!-- and framed versions of the glossary -->
<xsl:template match="glossary" mode="toc">
  <xsl:param name="toc-context" select="."/>

  <xsl:element name="{$toc.listitem.type}">
    <xsl:variable name="label">
      <xsl:apply-templates select="." mode="label.markup"/>
    </xsl:variable>
    <xsl:copy-of select="$label"/>
    <xsl:if test="$label != ''">
      <xsl:value-of select="$autotoc.label.separator"/>
    </xsl:if>

    <a>
      <xsl:attribute name="href">
        <xsl:call-template name="href.target">
          <xsl:with-param name="context" select="$toc-context"/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:apply-templates select="." mode="title.markup"/>
    </a>
  </xsl:element>
</xsl:template>

</xsl:stylesheet>
