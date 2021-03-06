<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<xsl:import href="jargon-chunk-base.xsl"/>
<xsl:import href="docbook-xsl-1.61.0/xhtml/chunk-common.xsl"/>
<xsl:include href="docbook-xsl-1.61.0/xhtml/manifest.xsl"/>

<xsl:include href="docbook-xsl-1.61.0/xhtml/chunk-code.xsl"/>

<xsl:template name="chunk">
  <xsl:param name="node" select="."/>
  <!-- returns 1 if $node is a chunk -->

  <!-- ==================================================================== -->
  <!-- What's a chunk?

       The root element
       appendix
       article
       bibliography  in article or book
       book
       chapter
       colophon
       glossary      in article or book
       index         in article or book
       part
       preface
       refentry
       reference
       sect{1,2,3,4,5}  if position()>1 && depth < chunk.section.depth
       section          if position()>1 && depth < chunk.section.depth
       set
       setindex
                                                                            -->
  <!-- ==================================================================== -->

<!--
  <xsl:message>
    <xsl:text>chunk: </xsl:text>
    <xsl:value-of select="name($node)"/>
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$node/@id"/>
    <xsl:text>)</xsl:text>
    <xsl:text> csd: </xsl:text>
    <xsl:value-of select="$chunk.section.depth"/>
    <xsl:text> cfs: </xsl:text>
    <xsl:value-of select="$chunk.first.sections"/>
    <xsl:text> ps: </xsl:text>
    <xsl:value-of select="count($node/parent::section)"/>
    <xsl:text> prs: </xsl:text>
    <xsl:value-of select="count($node/preceding-sibling::section)"/>
  </xsl:message>
-->

  <xsl:choose>
    <xsl:when test="not($node/parent::*)">1</xsl:when>

    <xsl:when test="local-name($node) = 'sect1'
                    and $chunk.section.depth &gt;= 1
                    and ($chunk.first.sections != 0
                         or count($node/preceding-sibling::sect1) &gt; 0)">
      <xsl:text>1</xsl:text>
    </xsl:when>
    <xsl:when test="local-name($node) = 'sect2'
                    and $chunk.section.depth &gt;= 2
                    and ($chunk.first.sections != 0
                         or count($node/preceding-sibling::sect2) &gt; 0)">
      <xsl:call-template name="chunk">
        <xsl:with-param name="node" select="$node/parent::*"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name($node) = 'sect3'
                    and $chunk.section.depth &gt;= 3
                    and ($chunk.first.sections != 0
                         or count($node/preceding-sibling::sect3) &gt; 0)">
      <xsl:call-template name="chunk">
        <xsl:with-param name="node" select="$node/parent::*"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name($node) = 'sect4'
                    and $chunk.section.depth &gt;= 4
                    and ($chunk.first.sections != 0
                         or count($node/preceding-sibling::sect4) &gt; 0)">
      <xsl:call-template name="chunk">
        <xsl:with-param name="node" select="$node/parent::*"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name($node) = 'sect5'
                    and $chunk.section.depth &gt;= 5
                    and ($chunk.first.sections != 0
                         or count($node/preceding-sibling::sect5) &gt; 0)">
      <xsl:call-template name="chunk">
        <xsl:with-param name="node" select="$node/parent::*"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name($node) = 'section'
                    and $chunk.section.depth &gt;= count($node/ancestor::section)+1
                    and ($chunk.first.sections != 0
                         or count($node/preceding-sibling::section) &gt; 0)">
      <xsl:call-template name="chunk">
        <xsl:with-param name="node" select="$node/parent::*"/>
      </xsl:call-template>
    </xsl:when>

    <xsl:when test="name($node)='preface'">1</xsl:when>
    <xsl:when test="name($node)='chapter'">1</xsl:when>
    <xsl:when test="name($node)='appendix'">1</xsl:when>
    <xsl:when test="name($node)='article'">1</xsl:when>
    <xsl:when test="name($node)='part'">1</xsl:when>
    <xsl:when test="name($node)='reference'">1</xsl:when>
    <xsl:when test="name($node)='refentry'">1</xsl:when>
    <xsl:when test="name($node)='index'
                    and (name($node/parent::*) = 'article'
                         or name($node/parent::*) = 'book')">1</xsl:when>
    <xsl:when test="name($node)='bibliography'
                    and (name($node/parent::*) = 'article'
                         or name($node/parent::*) = 'book')">1</xsl:when>
    <xsl:when test="name($node)='glossary'">1</xsl:when>
    <xsl:when test="name($node)='glossdiv'">1</xsl:when>
    <xsl:when test="name($node)='glossentry'">1</xsl:when>
    <xsl:when test="name($node)='colophon'">1</xsl:when>
    <xsl:when test="name($node)='book'">1</xsl:when>
    <xsl:when test="name($node)='set'">1</xsl:when>
    <xsl:when test="name($node)='setindex'">1</xsl:when>
    <xsl:otherwise>0</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="glossdiv" mode="recursive-chunk-filename">
  <xsl:param name="recursive" select="false()"/>

  <xsl:variable name="title">
    <xsl:choose>
      <xsl:when test="@xreflabel">
        <xsl:value-of select="@xreflabel"/>
      </xsl:when>
      <xsl:when test="titleabbrev">
        <xsl:value-of select="normalize-space(titleabbrev)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="normalize-space(title)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:value-of select="translate($title, ' /\!?,.()[]{}', '-')"/>

  <xsl:if test="not($recursive)">
    <xsl:value-of select="$html.ext"/>
  </xsl:if>
</xsl:template>

<xsl:template match="glossentry" mode="recursive-chunk-filename">
  <xsl:param name="recursive" select="false()"/>

  <xsl:variable name="title">
    <xsl:choose>
      <!-- This should really be conditional on use.id.as.filename --> 
      <xsl:when test="@id">
        <xsl:value-of select="@id"/>
      </xsl:when>
      <xsl:when test="@xreflabel">
        <xsl:value-of select="@xreflabel"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="normalize-space(glossterm[1])"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:if test="ancestor::glossdiv">
    <xsl:apply-templates select="parent::*" mode="recursive-chunk-filename">
      <xsl:with-param name="recursive" select="true()"/>
    </xsl:apply-templates>
    <xsl:text>/</xsl:text>
  </xsl:if>

  <xsl:value-of select="translate($title, ' /\!?.,()[]{}', '-')"/>

  <xsl:if test="not($recursive)">
    <xsl:value-of select="$html.ext"/>
  </xsl:if>
</xsl:template>

<xsl:template match="glossary|glossdiv|glossentry">
  <xsl:choose>
    <xsl:when test="$onechunk != 0 and parent::*">
      <xsl:apply-imports/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="process-chunk-element"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="chunk-first-section-with-parent">
  <xsl:param name="content">
    <xsl:apply-imports/>
  </xsl:param>

  <!-- These xpath expressions are really hairy. The trick is to pick sections -->
  <!-- that are not first children and are not the children of first children -->

  <!-- Break these variables into pieces to work around
       http://nagoya.apache.org/bugzilla/show_bug.cgi?id=6063 -->

  <xsl:variable name="prev-v1"
     select="(ancestor::glossdiv
             |ancestor::glossary

             |ancestor::sect1[$chunk.section.depth &gt; 0
                               and preceding-sibling::sect1][1]

             |ancestor::sect2[$chunk.section.depth &gt; 1
                               and preceding-sibling::sect2
                               and parent::sect1[preceding-sibling::sect1]][1]

             |ancestor::sect3[$chunk.section.depth &gt; 2
                               and preceding-sibling::sect3
                               and parent::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |ancestor::sect4[$chunk.section.depth &gt; 3
                               and preceding-sibling::sect4
                               and parent::sect3[preceding-sibling::sect2]
                               and ancestor::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |ancestor::sect5[$chunk.section.depth &gt; 4
                               and preceding-sibling::sect5
                               and parent::sect4[preceding-sibling::sect4]
                               and ancestor::sect3[preceding-sibling::sect3]
                               and ancestor::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |ancestor::section[$chunk.section.depth &gt; count(ancestor::section)
                                and not(ancestor::section[not(preceding-sibling::section)])][1])[last()]"/>

  <xsl:variable name="prev-v2"
     select="(preceding::glossdiv[1]
             |preceding::glossentry[1]

             |preceding::sect1[$chunk.section.depth &gt; 0
                               and preceding-sibling::sect1][1]

             |preceding::sect2[$chunk.section.depth &gt; 1
                               and preceding-sibling::sect2
                               and parent::sect1[preceding-sibling::sect1]][1]

             |preceding::sect3[$chunk.section.depth &gt; 2
                               and preceding-sibling::sect3
                               and parent::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |preceding::sect4[$chunk.section.depth &gt; 3
                               and preceding-sibling::sect4
                               and parent::sect3[preceding-sibling::sect2]
                               and ancestor::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |preceding::sect5[$chunk.section.depth &gt; 4
                               and preceding-sibling::sect5
                               and parent::sect4[preceding-sibling::sect4]
                               and ancestor::sect3[preceding-sibling::sect3]
                               and ancestor::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |preceding::section[$chunk.section.depth &gt; count(ancestor::section)
                                 and preceding-sibling::section
                                 and not(ancestor::section[not(preceding-sibling::section)])][1])[last()]"/>

  <xsl:variable name="prev"
    select="(preceding::book[1]
             |preceding::preface[1]
             |preceding::chapter[1]
             |preceding::appendix[1]
             |preceding::part[1]
             |preceding::reference[1]
             |preceding::refentry[1]
             |preceding::colophon[1]
             |preceding::article[1]
             |preceding::bibliography[1]
             |preceding::glossary[1]
             |preceding::index[1]
             |preceding::setindex[1]
             |ancestor::set
             |ancestor::book[1]
             |ancestor::preface[1]
             |ancestor::chapter[1]
             |ancestor::appendix[1]
             |ancestor::part[1]
             |ancestor::reference[1]
             |ancestor::article[1]
             |$prev-v1
             |$prev-v2)[last()]"/>

  <xsl:variable name="next-v1"
    select="(following::glossdiv[1]
             |following::glossentry[1]

             |following::sect1[$chunk.section.depth &gt; 0
                               and preceding-sibling::sect1][1]

             |following::sect2[$chunk.section.depth &gt; 1
                               and preceding-sibling::sect2
                               and parent::sect1[preceding-sibling::sect1]][1]

             |following::sect3[$chunk.section.depth &gt; 2
                               and preceding-sibling::sect3
                               and parent::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |following::sect4[$chunk.section.depth &gt; 3
                               and preceding-sibling::sect4
                               and parent::sect3[preceding-sibling::sect2]
                               and ancestor::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |following::sect5[$chunk.section.depth &gt; 4
                               and preceding-sibling::sect5
                               and parent::sect4[preceding-sibling::sect4]
                               and ancestor::sect3[preceding-sibling::sect3]
                               and ancestor::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |following::section[$chunk.section.depth &gt; count(ancestor::section)
                                 and preceding-sibling::section
                                 and not(ancestor::section[not(preceding-sibling::section)])][1])[1]"/>

  <xsl:variable name="next-v2"
    select="(descendant::glossdiv[1]
             |descendant::glossentry[1]

             |descendant::sect1[$chunk.section.depth &gt; 0
                               and preceding-sibling::sect1][1]

             |descendant::sect2[$chunk.section.depth &gt; 1
                               and preceding-sibling::sect2
                               and parent::sect1[preceding-sibling::sect1]][1]

             |descendant::sect3[$chunk.section.depth &gt; 2
                               and preceding-sibling::sect3
                               and parent::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |descendant::sect4[$chunk.section.depth &gt; 3
                               and preceding-sibling::sect4
                               and parent::sect3[preceding-sibling::sect2]
                               and ancestor::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |descendant::sect5[$chunk.section.depth &gt; 4
                               and preceding-sibling::sect5
                               and parent::sect4[preceding-sibling::sect4]
                               and ancestor::sect3[preceding-sibling::sect3]
                               and ancestor::sect2[preceding-sibling::sect2]
                               and ancestor::sect1[preceding-sibling::sect1]][1]

             |descendant::section[$chunk.section.depth &gt; count(ancestor::section)
                                 and preceding-sibling::section
                                 and not(ancestor::section[not(preceding-sibling::section)])])[1]"/>

  <xsl:variable name="next"
    select="(following::book[1]
             |following::preface[1]
             |following::chapter[1]
             |following::appendix[1]
             |following::part[1]
             |following::reference[1]
             |following::refentry[1]
             |following::colophon[1]
             |following::bibliography[1]
             |following::glossary[1]
             |following::index[1]
             |following::article[1]
             |following::setindex[1]
             |descendant::book[1]
             |descendant::preface[1]
             |descendant::chapter[1]
             |descendant::appendix[1]
             |descendant::article[1]
             |descendant::bibliography[1]
             |descendant::glossary[1]
             |descendant::index[1]
             |descendant::colophon[1]
             |descendant::setindex[1]
             |descendant::part[1]
             |descendant::reference[1]
             |descendant::refentry[1]
             |$next-v1
             |$next-v2)[1]"/>

  <xsl:call-template name="process-chunk">
    <xsl:with-param name="prev" select="$prev"/>
    <xsl:with-param name="next" select="$next"/>
    <xsl:with-param name="content" select="$content"/>
  </xsl:call-template>
</xsl:template>

<xsl:template name="chunk-all-sections">
  <xsl:param name="content">
    <xsl:apply-imports/>
  </xsl:param>

  <xsl:variable name="prev-v1"
    select="(preceding::sect1[$chunk.section.depth &gt; 0][1]
             |preceding::sect2[$chunk.section.depth &gt; 1][1]
             |preceding::sect3[$chunk.section.depth &gt; 2][1]
             |preceding::sect4[$chunk.section.depth &gt; 3][1]
             |preceding::sect5[$chunk.section.depth &gt; 4][1]
             |preceding::section[$chunk.section.depth &gt; count(ancestor::section)][1])[last()]"/>

  <xsl:variable name="prev-v2"
    select="(ancestor::sect1[$chunk.section.depth &gt; 0][1]
             |ancestor::sect2[$chunk.section.depth &gt; 1][1]
             |ancestor::sect3[$chunk.section.depth &gt; 2][1]
             |ancestor::sect4[$chunk.section.depth &gt; 3][1]
             |ancestor::sect5[$chunk.section.depth &gt; 4][1]
             |ancestor::section[$chunk.section.depth &gt; count(ancestor::section)][1])[last()]"/>

  <xsl:variable name="prev"
    select="(preceding::glossdiv[1]
             |preceding::glossentry[1]
             |ancestor::glossdiv[1]

             |preceding::book[1]
             |preceding::preface[1]
             |preceding::chapter[1]
             |preceding::appendix[1]
             |preceding::part[1]
             |preceding::reference[1]
             |preceding::refentry[1]
             |preceding::colophon[1]
             |preceding::article[1]
             |preceding::bibliography[1]
             |preceding::glossary[1]
             |preceding::index[1]
             |preceding::setindex[1]
             |ancestor::set
             |ancestor::book[1]
             |ancestor::preface[1]
             |ancestor::chapter[1]
             |ancestor::appendix[1]
             |ancestor::part[1]
             |ancestor::reference[1]
             |ancestor::article[1]
             |$prev-v1
             |$prev-v2)[last()]"/>

  <xsl:variable name="next-v1"
    select="(following::sect1[$chunk.section.depth &gt; 0][1]
             |following::sect2[$chunk.section.depth &gt; 1][1]
             |following::sect3[$chunk.section.depth &gt; 2][1]
             |following::sect4[$chunk.section.depth &gt; 3][1]
             |following::sect5[$chunk.section.depth &gt; 4][1]
             |following::section[$chunk.section.depth &gt; count(ancestor::section)][1])[1]"/>

  <xsl:variable name="next-v2"
    select="(descendant::sect1[$chunk.section.depth &gt; 0][1]
             |descendant::sect2[$chunk.section.depth &gt; 1][1]
             |descendant::sect3[$chunk.section.depth &gt; 2][1]
             |descendant::sect4[$chunk.section.depth &gt; 3][1]
             |descendant::sect5[$chunk.section.depth &gt; 4][1]
             |descendant::section[$chunk.section.depth 
                                  &gt; count(ancestor::section)][1])[1]"/>

  <xsl:variable name="next"
    select="(following::glossdiv[1]
             |following::glossentry[1]
             |descendant::glossdiv[1]
             |descendant::glossentry[1]

             |following::book[1]
             |following::preface[1]
             |following::chapter[1]
             |following::appendix[1]
             |following::part[1]
             |following::reference[1]
             |following::refentry[1]
             |following::colophon[1]
             |following::bibliography[1]
             |following::glossary[1]
             |following::index[1]
             |following::article[1]
             |following::setindex[1]
             |descendant::book[1]
             |descendant::preface[1]
             |descendant::chapter[1]
             |descendant::appendix[1]
             |descendant::article[1]
             |descendant::bibliography[1]
             |descendant::glossary[1]
             |descendant::index[1]
             |descendant::colophon[1]
             |descendant::setindex[1]
             |descendant::part[1]
             |descendant::reference[1]
             |descendant::refentry[1]
             |$next-v1
             |$next-v2)[1]"/>

  <xsl:call-template name="process-chunk">
    <xsl:with-param name="prev" select="$prev"/>
    <xsl:with-param name="next" select="$next"/>
    <xsl:with-param name="content" select="$content"/>
  </xsl:call-template>
</xsl:template>

</xsl:stylesheet>
