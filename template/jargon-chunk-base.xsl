<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<xsl:import href="jargon.xsl"/>

<!-- Support glossary as a chunk; add a simple ToC; write frames -->
<xsl:template match="glossary">
  <xsl:variable name="here" select="."/>

  <div class="{name(.)}">
    <xsl:if test="$generate.id.attributes != 0">
      <xsl:attribute name="id">
        <xsl:call-template name="object.id"/>
      </xsl:attribute>
    </xsl:if>

    <xsl:if test="$suppress.glossary.titlepage = 0">
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

  <!-- Now make the frames -->
  <xsl:call-template name="write.chunk">
    <xsl:with-param name="filename" select="concat($base.dir,'frames.html')"/>
    <xsl:with-param name="content">
      <html>
      <frameset cols="32%,*">
        <frameset rows="20%,*">
          <frame name="letters_frame" src="letters.html"/>
          <frame name="headwords_frame" src="0-frame.html"/>
        </frameset>
        <frame name="entry_frame" src="0/dev-null.html"/>
        <noframes>
          <body>
            <p>Click <a href="letters.html">here</a> for a non-frames version.</p>
          </body>
        </noframes>
      </frameset>
      </html>
    </xsl:with-param>
  </xsl:call-template>

  <xsl:call-template name="write.chunk">

    <!-- Generate the letters frame that indexes the glossary divisions -->
    <xsl:with-param name="filename" select="concat($base.dir,'letters.html')"/>
    <xsl:with-param name="content">
      <html>
        <head>
          <title>Letters</title>
          <xsl:if test="$html.stylesheet != ''">
            <xsl:call-template name="output.html.stylesheets">
              <xsl:with-param name="stylesheets" select="normalize-space($html.stylesheet)"/>
            </xsl:call-template>
          </xsl:if>
       </head>
        <body>
          <p>
            <b>
              <!-- Use the title and subtitle immediately under the -->
              <!-- document top node to decorate the letters frame -->
	      <img src='../graphics/glider-small.png' align='left'/>
              <xsl:value-of select="/*/title"/>
              <br/>
              <xsl:value-of select="/*/subtitle"/>
            </b>
          </p>
          <hr/>
          <p>
            <a href="index.html" target="_parent">Up</a>
          </p>
          <p>
            <xsl:for-each select="glossdiv">
              <a target="headwords_frame">
                <xsl:attribute name="href">
                  <xsl:apply-templates select="." mode="recursive-chunk-filename">
                    <xsl:with-param name="recursive" select="true()"/>
                  </xsl:apply-templates>
                  <xsl:text>-frame</xsl:text>
                  <xsl:value-of select="$html.ext"/>
                </xsl:attribute>
                <xsl:value-of select="title"/>
              </a>
              <xsl:text> </xsl:text>
            </xsl:for-each>
          </p>
        </body>
      </html>
    </xsl:with-param>
  </xsl:call-template>

  <!-- Generate a node listing for each glossary division -->
  <xsl:for-each select="glossdiv">
    <xsl:call-template name="write.chunk">
      <xsl:with-param name="filename">
        <xsl:value-of select="$base.dir"/>
        <xsl:apply-templates select="." mode="recursive-chunk-filename">
          <xsl:with-param name="recursive" select="true()"/>
        </xsl:apply-templates>
        <xsl:text>-frame</xsl:text>
        <xsl:value-of select="$html.ext"/>
      </xsl:with-param>
      <xsl:with-param name="content">
        <html>
          <head>
            <title><xsl:value-of select="title"/></title>
            <xsl:if test="$html.stylesheet != ''">
              <xsl:call-template name="output.html.stylesheets">
                <xsl:with-param name="stylesheets" select="normalize-space($html.stylesheet)"/>
              </xsl:call-template>
            </xsl:if>
          </head>
          <body>
            <xsl:for-each select="glossentry">
              <a target="entry_frame">
                <xsl:attribute name="href">
                  <xsl:call-template name="href.target">
                    <xsl:with-param name="context" select="$here"/>
                  </xsl:call-template>
                </xsl:attribute>
                <xsl:value-of select="glossterm[1]"/>
              </a>
              <br/>
            </xsl:for-each>
          </body>
        </html>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
