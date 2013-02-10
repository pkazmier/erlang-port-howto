<?xml version="1.0" encoding="UTF-8"?>

<!-- For local usage only -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output encoding="UTF-8" method="html" indent="yes" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"/>

<!-- When using <pre>, whitespaces should be preserved -->
<xsl:preserve-space elements="pre"/>

<!-- Global definition of style parameter -->
<xsl:param name="style">0</xsl:param>

<!-- img tag -->
<xsl:template match="img">
  <img src="{@src}"/>
</xsl:template>

<!-- Content of /guide -->
<xsl:template name="guidecontent">
  <br/>
  <p class="dochead">
    <xsl:choose>
      <xsl:when test="/guide/subtitle"><xsl:value-of select="/guide/title"/>: <xsl:value-of select="/guide/subtitle"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="/guide/title"/></xsl:otherwise>
    </xsl:choose>
  </p>

  <xsl:if test="$style = 'printable'">
    <xsl:apply-templates select="author" />
  </xsl:if>

  <form name="contents" action="http://www.gentoo.org">
    <b>Contents</b>:
    <select name="url" size="1" OnChange="location.href=form.url.options[form.url.selectedIndex].value" style="font-family:Arial,Helvetica, sans-serif; font-size:10">
      <xsl:for-each select="chapter">
        <xsl:variable name="chapid">doc_chap<xsl:number/></xsl:variable><option value="#{$chapid}"><xsl:number/>. <xsl:value-of select="title"/></option>
      </xsl:for-each>
    </select>
  </form>
  <xsl:apply-templates select="chapter"/>
  <br/>
  <xsl:if test="/guide/license">
    <xsl:apply-templates select="license" />
  </xsl:if>
  <br/>
</xsl:template>

<!-- Layout for documentation -->
<xsl:template name="doclayout">
<html>
<head>
<link title="new" rel="stylesheet" href="css/main.css" type="text/css"/>
<link REL="shortcut icon" HREF="favicon.ico" TYPE="image/x-icon"/>
<title>Erlang HowTo 
  <xsl:choose>
    <xsl:when test="/guide/@type='project'">Projects</xsl:when>
    <xsl:when test="/guide/@type='newsletter'">Newsletter</xsl:when>
    <xsl:otherwise>Documentation</xsl:otherwise>
  </xsl:choose>
-- 
  <xsl:choose>
    <xsl:when test="subtitle"><xsl:if test="/guide/@type!='newsletter'"><xsl:value-of select="title"/>:</xsl:if> <xsl:value-of select="subtitle"/></xsl:when>
    <xsl:otherwise><xsl:value-of select="title"/></xsl:otherwise>
  </xsl:choose>
</title>
</head>
<body style="margin-left:0px;margin-top:0px;" bgcolor="#ffffff">

<table width="100%" border="0" cellspacing="0" cellpadding="0">
  <tr>
    <td valign="top" height="125" bgcolor="#6699ff">
      <table cellspacing="0" cellpadding="0" border="0" width="193">
        <tr>
          <td class="logobg" valign="top" align="center" height="88">
            <a href="/"><img border="0" src="images/trapexit.gif" alt="trapexit.org Logo"/></a>
          </td>
        </tr>
        <tr>
          <td class="logobg" valign="top" align="center" height="36">
            <a href="/"></a>
          </td>
        </tr>
      </table>
    </td>
  </tr>
  <tr>
    <td valign="top" align="right" colspan="1" bgcolor="#ffffff">
      <table border="0" cellspacing="0" cellpadding="0" width="100%">
        <tr>
          <td width="99%" class="content" valign="top" align="left">
            <!-- Insert the node-specific content -->
            <xsl:call-template name="content"/>
          </td>
          <td width="1%" bgcolor="#dddaec" valign="top">
            <table border="0" cellspacing="5" cellpadding="0">
              <tr>
                <td>
                  <img src="images/line.gif" alt="line"/>
                </td>
              </tr>
              <tr>
                <td align="center" class="alttext">
                  <!-- Update datestamp -->
                  Updated <xsl:value-of select="/guide/date|/book/date"/>
                </td>
              </tr>
              <tr>
                <td>
                  <img src="images/line.gif" alt="line"/>
                </td>
              </tr>
              <tr>
                <td class="alttext">
                  <!-- Authors -->
                  <xsl:apply-templates select="/guide/author|/book/author"/>
                </td>
              </tr>
              <tr>
                <td>
                  <img src="images/line.gif" alt="line"/>
                </td>
              </tr>
              <tr>
                <td class="alttext">
                  <!-- Abstract (summary) of the document -->
                  <b>Summary:</b>&#160;<xsl:apply-templates select="abstract"/>
                </td>
              </tr>
              <tr>
                <td>
                  <img src="images/line.gif" alt="line"/>
                </td>
              </tr>
            </table>
          </td>
        </tr>
      </table>
    </td>
  </tr>
</table>

</body>
</html>
</xsl:template>

<!-- Guide template -->
<xsl:template match="/guide">
<xsl:call-template name="doclayout" />
</xsl:template>

<!-- {Mainpage, News, Email} template -->
<xsl:template match="/mainpage | /news | /email">
<html>
<head>
  <link title="new" rel="stylesheet" href="css/main.css" type="text/css"/>
  <link REL="shortcut icon" HREF="/favicon.ico" TYPE="image/x-icon"/>
  <xsl:if test="/mainpage/@id='news'">
    <link rel="alternate" type="application/rss+xml" title="Gentoo Linux News RDF" href="http://www.gentoo.org/rdf/en/gentoo-news.rdf" />
  </xsl:if>
  <xsl:choose>
    <xsl:when test="/mainpage | /news">
      <title>Gentoo Linux -- <xsl:value-of select="title"/></title>
    </xsl:when>
    <xsl:when test="/email">
      <title><xsl:value-of select="subject"/></title>
    </xsl:when>
  </xsl:choose>
</head>
<body style="margin-left:0px;margin-top:0px;" bgcolor="#000000">

<table border="0" width="100%" cellspacing="0" cellpadding="0">
  <tr>
    <td valign="top" height="125" width="1%" bgcolor="#6699ff">
      <table cellspacing="0" cellpadding="0" border="0" width="100%">
        <tr>
          <td class="logobg" valign="top" align="center" height="88">
            <a href="/">
              <img border="0" src="images/gtop-s.jpg" alt="Gentoo Logo"/>
            </a>
          </td>
        </tr>
        <tr>
          <td class="logobg" valign="top" align="center" height="36">
            <a href="/">
              <img border="0" src="images/gbot-s.gif" alt="Gentoo Logo Side"/>
            </a>
          </td>
        </tr>
      </table>
    </td>
    <td valign="bottom" align="left" bgcolor="#000000" colspan="2">
      <p class="menu">
        <xsl:choose>
          <xsl:when test="/mainpage/@id='about'">
            <a class="highlight" href="/main/en/about.xml">About</a> |
          </xsl:when>
          <xsl:otherwise>
            <a class="menulink" href="/main/en/about.xml">About</a> |
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="/mainpage/@id='projects'">
            <a class="highlight" href="/proj/en/metastructure/projects.xml?showlevel=1">Projects</a> |
          </xsl:when>
          <xsl:otherwise>
            <a class="menulink" href="/proj/en/metastructure/projects.xml?showlevel=1">Projects</a> |
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="/mainpage/@id='contract'">
            <a class="highlight" href="/main/en/philosophy.xml">Philosophy</a> |
          </xsl:when>
          <xsl:otherwise>
            <a class="menulink" href="/main/en/philosophy.xml">Philosophy</a> |
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="/mainpage/@id='docs'">
            <a class="highlight" href="/doc/en/index.xml">Docs</a> |
          </xsl:when>
          <xsl:otherwise>
            <a class="menulink" href="/doc/en/index.xml">Docs</a> |
          </xsl:otherwise>
        </xsl:choose>
        <a class="menulink" href="http://forums.gentoo.org">Forums</a> |
        <xsl:choose>
          <xsl:when test="/mainpage/@id='lists'">
            <a class="highlight" href="/main/en/lists.xml">Lists</a> |
          </xsl:when>
          <xsl:otherwise>
            <a class="menulink" href="/main/en/lists.xml">Lists</a> |
          </xsl:otherwise>
        </xsl:choose>
        <a class="menulink" href="http://bugs.gentoo.org">Bugs</a> |
        <a class="menulink" href="http://store.gentoo.org">Store</a> |
        <xsl:choose>
          <xsl:when test="/mainpage/@id='newsletter'">
            <a class="highlight" href="/news/en/gwn/gwn.xml"> GWN</a> |
          </xsl:when>
          <xsl:otherwise>
            <a class="menulink" href="/news/en/gwn/gwn.xml"> GWN</a> |
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="/mainpage/@id='where'">
            <a class="highlight" href="/main/en/where.xml">Get Gentoo!</a> |
          </xsl:when>
          <xsl:otherwise>
            <a class="menulink" href="/main/en/where.xml">Get Gentoo!</a> |
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="/mainpage/@id='sponsors'">
            <a class="highlight" href="/main/en/sponsors.xml">Sponsors</a>
          </xsl:when>
          <xsl:otherwise>
            <a class="menulink" href="/main/en/sponsors.xml">Sponsors</a> 
          </xsl:otherwise>
        </xsl:choose>
      </p>
    </td>
  </tr>
  <tr>
    <td valign="top" align="right" width="1%" bgcolor="#dddaec">
      <table width="100%" cellspacing="0" cellpadding="0" border="0">
        <tr>
          <td height="1%" valign="top" align="right">
            <img src="images/gridtest.gif" alt="Gentoo Spaceship"/>
          </td>
        </tr>
        <tr>
          <td height="99%" valign="top" align="right">
            <!--info goes here-->
            <table cellspacing="0" cellpadding="5" border="0">
              <tr>
                <td valign="top">
                  <p class="altmenu">
                    Documentation:
                    <br/>
                    <a class="altlink" href="/main/en/about.xml">About Gentoo Linux</a>
                    <br/>
                    <a class="altlink" href="/doc/en/index.xml">User Docs</a>
                    <br/>
                    <a class="altlink" href="/doc/en/index.xml#doc_chap6">Developer Docs</a>
                    <br/>
                    <a class="altlink" href="/doc/en/index.xml#doc_chap7">Other Docs/Translations</a>
                    <br/>
                    <a class="altlink" href="/main/en/philosophy.xml">Philosophy</a>
                    <br/><br/>
                    Installation:
                    <br/>
                    <a class="altlink" href="/doc/en/handbook/handbook.xml?part=1">Gentoo Handbook Installation Instructions</a>
                    <br/><br/>
                    Resources:
                    <br/>
                    <a class="altlink" href="/main/en/lists.xml">Mailing lists</a>
                    <br/>
                    <a class="hotlink" href="/main/en/performance.xml">Performance benchmarks</a>
                    <br/>
                    <a class="altlink" href="http://forums.gentoo.org">Discussion forums</a>
                    <br/>
                    <a class="altlink" href="/dyn/index-cvs.xml">Daily CVS ChangeLog</a>
                    <br/>
                    <a class="altlink" href="/main/en/devlist.xml">Developer List</a>
                    <br/>
                    <a class="altlink" href="http://bugs.gentoo.org">Bugzilla bug tracker</a>
                    <br/>
                    <a class="altlink" href="/main/en/mirrors.xml">Download Mirrors</a>
                    <br/>
                    <a class="altlink" href="/main/en/irc.xml">Official Gentoo IRC channels</a>
                    <br/>
                    <a class="altlink" href="http://packages.gentoo.org/">Online package database</a>
                    <br/>
                    <a class="altlink" href="http://www.gentoo.org/cgi-bin/viewcvs.cgi">View our CVS via the web</a>
                    <br/>
                    <!--<a class="altlink" href="http://stats.gentoo.org">Gentoo Usage Statistics</a>
                    <br/>
                    <a class="altlink" href="http://stable.gentoo.org">Gentoo Stable Project</a>
                    <br/>
                    -->
                    <br/><br/>
                    Graphics:
                    <br/>
                    <a class="altlink" href="/main/en/graphics.xml">Logos and themes</a>
                    <br/>
                    <a class="altlink" href="/dyn/icons.xml">Icons</a>
                    <br/>
                    <a class="altlink" href="/main/en/shots.xml">ScreenShots</a>
                    <br/><br/>	
                    Miscellaneous Resources:
                    <br/>
                    <a class="altlink" href="http://store.gentoo.org">Gentoo Linux Store</a>
                    <br/>
                    <a class="altlink" href="/main/en/projects.xml">Gentoo-hosted projects</a>
                    <br/>
                    <a class="altlink" href="/main/en/articles.xml">IBM dW/Intel article archive</a>
                    <xsl:if test="/mainpage/@id='news'">
                      <br/><br/>
                      Older News:<br/>
                      <xsl:for-each select="document('/dyn/news-index.xml')/uris/uri[position()&gt;=7][position()&lt;20]/text()">
                        <xsl:variable name="newsuri" select="."/>
                        <a class="altlink" href="{$newsuri}"><xsl:value-of select="document(.)/news/title"/></a><br/>
                      </xsl:for-each>
                    </xsl:if>
                  </p>
                  <br/><br />
                </td>
              </tr>
            </table>
          </td>
        </tr>
      </table>
    </td>
    <td valign="top" align="right" bgcolor="#ffffff">
      <table border="0" cellspacing="5" cellpadding="0" width="100%">
        <tr>
          <td valign="top" align="left">
            <xsl:choose>
              <xsl:when test="/mainpage/@id='news'">
                <table class="content" cellpadding="4" width="100%" border="0">
                  <tr>
                    <td valign="top">
                      <img src="images/gentoo-new.gif" alt="new"/>
                    </td>
                    <td valign="middle">
                      We produce Gentoo Linux, a special flavor of Linux that 
                      can be automatically optimized and customized for just 
                      about any application or need. Extreme performance, 
                      configurability and a top-notch user and developer 
                      community are all hallmarks of the Gentoo experience.
                      To learn more, <b><a href="/main/en/about.xml">click 
                      here</a></b>.
                    </td>
                  </tr>
                </table>
                <br/>
                <xsl:for-each select="document('/dyn/news-index.xml')/uris/uri[position()&lt;7]/text()">
                  <table class="content" cellpadding="4" width="100%" border="0">
                    <tr>
                      <td colspan="2" bgcolor="#7a5ada">
                        <font color="#ffffff">
                          <b>
                            <xsl:value-of select="document(.)/news/title"/>
                          </b>
                          <br/>
                          <font size="-3">Posted on <xsl:value-of select="document(.)/news/date"/> by <xsl:value-of select="document(.)/news/poster"/></font>
                        </font>
                      </td>
                    </tr>
                    <tr>
                      <td width="100" align="center" valign="middle">
                        <xsl:choose>
                          <xsl:when test="document(.)/news/@category='alpha'">
                            <img src="images/icon-alpha.gif" alt="AlphaServer GS160"/>
                          </xsl:when>
                          <xsl:when test="document(.)/news/@category='kde'">
                            <img src="images/icon-kde.png" alt="KDE"/>
                          </xsl:when>
                          <xsl:when test="document(.)/news/@category='gentoo'">
                            <img src="images/icon-gentoo.png" alt="gentoo"/>
                          </xsl:when>
                          <xsl:when test="document(.)/news/@category='main'">
                            <img src="images/icon-stick.png" alt="stick man"/>
                          </xsl:when>
                          <xsl:when test="document(.)/news/@category='ibm'">
                            <img src="images/icon-ibm.gif" alt="ibm"/>
                          </xsl:when>
                          <xsl:when test="document(.)/news/@category='linux'">
                            <img src="images/icon-penguin.png" alt="tux"/>
                          </xsl:when>
                          <xsl:when test="document(.)/news/@category='moo'">
                            <img src="images/icon-cow.png" alt="Larry the Cow"/>
                          </xsl:when>
                          <xsl:when test="document(.)/news/@category='plans'">
                            <img src="images/icon-clock.png" alt="Clock"/>
                          </xsl:when>
                          <xsl:when test="document(.)/news/@category='nvidia'">
                            <img src="images/icon-nvidia.png" alt="Nvidia"/>
                          </xsl:when>
                        </xsl:choose>
                      </td>
                      <td valign="top">
                        <xsl:choose>
                          <xsl:when test="document(.)/news/summary">
                            <xsl:apply-templates select="document(.)/news/summary"/>
                            <br/>
                            <a href="{@external}"><b>(full story)</b></a>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:apply-templates select="document(.)/news/body"/>
                          </xsl:otherwise>
                        </xsl:choose>
                      </td>
                    </tr>
                  </table>
                  <br/>
                </xsl:for-each>
              </xsl:when>
              <xsl:when test="/news">
                <table class="content" cellpadding="4" width="100%" border="0">
                  <tr>
                    <td colspan="2" bgcolor="#7a5ada">
                      <font color="#ffffff">
                        <b><xsl:value-of select="title"/></b>
                      </font>
                      <br/>
                      <font size="-3">Posted on <xsl:value-of select="date"/> by <xsl:value-of select="poster"/></font>
                    </td>
                  </tr>
                  <tr>
                    <td width="100" align="center" valign="top">
                      <xsl:choose>
                        <xsl:when test="@category='alpha'">
                          <img src="images/icon-alpha.gif" alt="AlphaServer GS160"/>
                        </xsl:when>
                        <xsl:when test="@category='kde'">
                          <img src="images/icon-kde.png" alt="KDE"/>
                        </xsl:when>
                        <xsl:when test="@category='gentoo'">
                          <img src="images/icon-gentoo.png" alt="gentoo"/>
                        </xsl:when>
                        <xsl:when test="@category='main'">
                          <img src="images/icon-stick.png" alt="stick man"/>
                        </xsl:when>
                        <xsl:when test="@category='ibm'">
                          <img src="images/icon-ibm.gif" alt="IBM"/>
                        </xsl:when>
                        <xsl:when test="@category='linux'">
                          <img src="images/icon-penguin.png" alt="Tux the Penguin"/>
                        </xsl:when>
                        <xsl:when test="@category='moo'">
                          <img src="images/icon-cow.png" alt="Larry the Cow"/>
                        </xsl:when>
                        <xsl:when test="@category='nvidia'">
                          <img src="images/icon-nvidia.png" alt="nvidia"/>
                        </xsl:when>
                      </xsl:choose>
                    </td>
                    <td valign="top">
                      <xsl:choose>
                        <xsl:when test="body">
                          <xsl:apply-templates select="body"/>
                        </xsl:when>
                        <xsl:when test="section">
                          <xsl:apply-templates select="section"/>
                        </xsl:when>
                      </xsl:choose>
                    </td>
                  </tr>
                </table>
              </xsl:when>
              <xsl:when test="/email">
                <xsl:apply-templates select="/email/body"/>
              </xsl:when>
              <xsl:otherwise>
                <br/>
                <table border="0" class="content">
                  <tr>
                    <td>
                      <xsl:apply-templates select="chapter"/>
                    </td>
                  </tr>
                </table>
                <br/>
                <br/>
                <br/>
              </xsl:otherwise>
            </xsl:choose>
            <!--content end-->
          </td>
        </tr>
      </table>
    </td>

  <!--
    <td width="15%" class="infotext" valign="top" align="left" bgcolor="#ddddff">
      <table border="0" cellspacing="5" cellpadding="0" width="100%">
        <tr>
          <td>
            <br/>
          </td>
        </tr>
      </table>
    </td>
  -->
  </tr>
  <tr>
    <td align="right" class="infohead" width="100%" colspan="3" bgcolor="#7a5ada">
      Copyright 2001-2003 Gentoo Technologies, Inc.  Questions, Comments, Corrections?  Email <a class="highlight" href="mailto:www@gentoo.org">www@gentoo.org</a>.
    </td>
  </tr>
</table>

</body>
</html>
</xsl:template>

<!-- News items -->
<xsl:template match="newsitems">
  <xsl:apply-templates select="news"/>
</xsl:template>

<!-- News template, child of newsitems -->
<xsl:template match="news">
<table width="100%" border="0" cellspacing="5" cellpadding="0">
  <tr>
    <td colspan="2" class="ncontent" bgcolor="#bbffbb">
      <p class="note">
        <font color="#7a5ada">
          <b><xsl:value-of select="title"/></b>
        </font>
      </p>
    </td>
  </tr>
  <tr>
    <xsl:choose>
      <xsl:when test="@align='left'">
        <td rowspan="2" valign="top" width="1">
          <img src="{@graphic}"/>
        </td>
        <td class="alttext">
          <font color="#808080">
            Posted by <xsl:value-of select="poster"/> on <xsl:value-of select="date"/>
          </font>
        </td>
      </xsl:when>
      <xsl:otherwise>
        <td class="alttext">
          <font color="#808080">
            Posted by <xsl:value-of select="poster"/> on <xsl:value-of select="date"/>
          </font>
        </td>
        <td rowspan="2" valign="top" width="1">
          <img src="{@graphic}"/>
        </td>
      </xsl:otherwise>
    </xsl:choose>
  </tr>
  <tr>
    <td class="content" valign="top">
      <xsl:apply-templates select="body"/>
    </td>
  </tr>
</table>
<br/>
<table width="100%">
  <tr>
    <td height="1" bgcolor="#c0c0c0"/>
  </tr>
</table>
<br/>
</xsl:template>

<!-- Mail template -->
<xsl:template match="mail">
<a href="mailto:{@link}"><xsl:value-of select="."/></a>
</xsl:template>

<!-- Mail inside <author>...</author> -->
<xsl:template match="/guide/author/mail|/book/author/mail">
<b>
  <a class="altlink" href="mailto:{@link}"><xsl:value-of select="."/></a>
</b>
</xsl:template>

<!-- Author -->
<xsl:template match="author">
<xsl:apply-templates/>
<xsl:if test="@title">
<xsl:if test="$style != 'printable'">
  <br/>
</xsl:if>
  <i><xsl:value-of select="@title"/></i>
</xsl:if>
<br/>
<xsl:if test="$style != 'printable'">
  <br/>
</xsl:if>
</xsl:template>

<!-- Chapter -->
<xsl:template match="chapter">
<xsl:variable name="chid"><xsl:number/></xsl:variable>
<xsl:choose>
  <xsl:when test="title">
    <p class="chaphead">
      <xsl:if test="@id">
        <a name="{@id}"/>
      </xsl:if>
      <span class="chapnum">
        <a name="doc_chap{$chid}"><xsl:number/>. </a>
      </span>
      <xsl:value-of select="title"/>
    </p>
  </xsl:when>
  <xsl:otherwise>
    <xsl:if test="/guide">
      <p class="chaphead">
        <span class="chapnum">
          <a name="doc_chap{$chid}"><xsl:number/>.</a>
        </span>
      </p>
    </xsl:if>
  </xsl:otherwise>
</xsl:choose>
<xsl:apply-templates select="body">
  <xsl:with-param name="chid" select="$chid"/>
</xsl:apply-templates>
<xsl:apply-templates select="section">
  <xsl:with-param name="chid" select="$chid"/>
</xsl:apply-templates>
</xsl:template>


<!-- Section template -->
<xsl:template match="section">
<xsl:param name="chid"/>
<xsl:if test="title">
  <xsl:variable name="sectid">doc_chap<xsl:value-of select="$chid"/>_sect<xsl:number/></xsl:variable>
  <xsl:if test="@id">
    <a name="{@id}"/>
  </xsl:if>
  <p class="secthead">
    <a name="{$sectid}"><xsl:value-of select="title"/>&#160;</a>
  </p>
</xsl:if>
<xsl:apply-templates select="body">
  <xsl:with-param name="chid" select="$chid"/>
</xsl:apply-templates>
</xsl:template>

<!-- Figure template -->
<xsl:template match="figure">
<xsl:param name="chid"/>
<xsl:variable name="fignum"><xsl:number level="any" from="chapter" count="figure"/></xsl:variable>
<xsl:variable name="figid">doc_chap<xsl:value-of select="$chid"/>_fig<xsl:value-of select="$fignum"/></xsl:variable>
<br/>
<a name="{$figid}"/>
<table cellspacing="0" cellpadding="0" border="0">
  <tr>
    <td class="infohead" bgcolor="#7a5ada">
      <p class="caption">
        <xsl:choose>
          <xsl:when test="@caption">
            Figure <xsl:value-of select="$chid"/>.<xsl:value-of select="$fignum"/>: <xsl:value-of select="@caption"/>
          </xsl:when>
          <xsl:otherwise>
            Figure <xsl:value-of select="$chid"/>.<xsl:value-of select="$fignum"/>
          </xsl:otherwise>
        </xsl:choose>
      </p>
    </td>
  </tr>
  <tr>
    <td align="center" bgcolor="#ddddff">
      <xsl:choose>
        <xsl:when test="@short">
          <img src="{@link}" alt="Fig. {$fignum}: {@short}"/>
        </xsl:when>
        <xsl:otherwise>
          <img src="{@link}" alt="Fig. {$fignum}"/>
        </xsl:otherwise>
      </xsl:choose>
    </td>
  </tr>
</table>
<br/>
</xsl:template>

<!--figure without a caption; just a graphical element-->
<xsl:template match="fig">
<center>
  <xsl:choose>
    <xsl:when test="@linkto">
      <a href="{@linkto}"><img border="0" src="{@link}" alt="{@short}"/></a>
    </xsl:when>
    <xsl:otherwise>
      <img src="{@link}" alt="{@short}"/>
    </xsl:otherwise>
  </xsl:choose>
</center>
</xsl:template>

<!-- Line break -->
<xsl:template match="br">
<br/>
</xsl:template>

<!-- Note -->
<xsl:template match="note">
<table class="ncontent" width="100%" border="0" cellspacing="0" cellpadding="0">
  <tr>
    <td bgcolor="#bbffbb">
      <p class="note">
        <b>Note: </b>
        <xsl:apply-templates/>
      </p>
    </td>
  </tr>
</table>
</xsl:template>

<!-- Important item -->
<xsl:template match="impo">
<table class="ncontent" width="100%" border="0" cellspacing="0" cellpadding="0">
  <tr>
    <td bgcolor="#ffffbb">
      <p class="note">
        <b>Important: </b>
        <xsl:apply-templates/>
      </p>
    </td>
  </tr>
</table>
</xsl:template>

<!-- Warning -->
<xsl:template match="warn">
<table class="ncontent" width="100%" border="0" cellspacing="0" cellpadding="0">
  <tr>
    <td bgcolor="#ffbbbb">
      <p class="note">
        <b>Warning: </b>
        <xsl:apply-templates/>
      </p>
    </td>
  </tr>
</table>
</xsl:template>

<!-- Code note -->
<xsl:template match="codenote">
<span class="comment">// <xsl:value-of select="."/></span>
</xsl:template>

<!-- Regular comment -->
<xsl:template match="comment">
<span class="comment">
  <xsl:apply-templates/>
</span>
</xsl:template>

<!-- User input -->
<xsl:template match="i">
<span class="input"><xsl:apply-templates/></span>
</xsl:template>

<!-- Bold -->
<xsl:template match="b">
<b><xsl:apply-templates/></b>
</xsl:template>

<!-- Brite -->
<xsl:template match="brite">
<font color="#ff0000">
  <b><xsl:apply-templates/></b>
</font>
</xsl:template>

<!-- Body inside email -->
<xsl:template match="/email/body">
<table border="0">
  <tr>
    <td>
      <span class="content">
        <p class="secthead">
          Subject: <xsl:value-of select="/email/subject"/>
        </p>
        <p class="secthead">
          <font color="#000000">
            List: <xsl:value-of select="/email/list"/> at gentoo.org<br/>
            Date: <xsl:value-of select="/email/date"/><br/>
            From: <xsl:value-of select="/email/from"/><br/><br/>
            <xsl:if test="/email/nav/prev">
              <xsl:for-each select="/email/nav/prev[position()=1]/text()">
                <xsl:variable name="navloc" select="."/>
                <xsl:variable name="navfile">/dyn/lists/<xsl:value-of select="/email/list"/>/<xsl:value-of select="."/>.xml</xsl:variable>
                Previous: <a href="{$navfile}"><xsl:value-of select="document($navfile)/email/subject"/></a><br/>
              </xsl:for-each>
            </xsl:if>
            <xsl:if test="/email/nav/next">
              <xsl:for-each select="/email/nav/next[position()=1]/text()">
                <xsl:variable name="navloc" select="."/>
                <xsl:variable name="navfile">/dyn/lists/<xsl:value-of select="/email/list"/>/<xsl:value-of select="."/>.xml</xsl:variable>
                Next: <a href="{$navfile}"><xsl:value-of select="document($navfile)/email/subject"/></a><br/>
              </xsl:for-each>
            </xsl:if>
            <xsl:if test="/email/in-reply-to">
              <xsl:for-each select="/email/in-reply-to[position()=1]/text()">
                <xsl:variable name="irtloc" select="."/>
                <xsl:variable name="irtfile">/dyn/lists/<xsl:value-of select="/email/list"/>/<xsl:value-of select="."/>.xml</xsl:variable>
                In Reply To: <a href="{$irtfile}"><xsl:value-of select="document($irtfile)/email/subject"/></a><br/>
              </xsl:for-each>
            </xsl:if>
            <xsl:if test="/email/replies">
              <br/>Replies to this message:<br/>
              <xsl:for-each select="/email/replies/reply/text()">
                <xsl:variable name="rloc" select="."/>
                <xsl:variable name="rfile">/dyn/lists/<xsl:value-of select="/email/list"/>/<xsl:value-of select="."/>.xml</xsl:variable>
                &#160;<a href="{$rfile}"><xsl:value-of select="document($rfile)/email/subject"/></a><br/>
              </xsl:for-each>
            </xsl:if>
          </font>
        </p>
      </span>
      <pre>
        <xsl:apply-templates/>
      </pre>
    </td>
  </tr>
</table>
</xsl:template>

<!-- Body -->
<xsl:template match="body">
<xsl:param name="chid"/>
<xsl:apply-templates>
  <xsl:with-param name="chid" select="$chid"/>
</xsl:apply-templates>
</xsl:template>

<!-- Command or input, not to use inside <pre> -->
<xsl:template match="c">
<span class="code"><xsl:apply-templates/></span>
</xsl:template>

<!-- Box with small text -->
<xsl:template match="box">
<p class="infotext"><xsl:apply-templates/></p>
</xsl:template>

<!-- Preserve whitespace, aka Code Listing -->
<xsl:template match="pre">
<xsl:param name="chid"/>
<xsl:variable name="prenum"><xsl:number level="any" from="chapter" count="pre"/></xsl:variable>
<xsl:variable name="preid">doc_chap<xsl:value-of select="$chid"/>_pre<xsl:value-of select="$prenum"/></xsl:variable>
<a name="{$preid}"/>
<table class="ntable" width="100%" cellspacing="0" cellpadding="0" border="0">
  <tr>
    <td class="infohead" bgcolor="#7a5ada">
      <p class="caption">
        <xsl:choose>
          <xsl:when test="@caption">
            Code listing <xsl:if test="$chid"><xsl:value-of select="$chid"/>.</xsl:if><xsl:value-of select="$prenum"/>: <xsl:value-of select="@caption"/>
          </xsl:when>
          <xsl:otherwise>
            Code listing <xsl:value-of select="$chid"/>.<xsl:value-of select="$prenum"/>
          </xsl:otherwise>
        </xsl:choose>
      </p>
    </td>
  </tr>
  <tr>
    <td bgcolor="#ddddff">
      <pre>
        <xsl:apply-templates/>
      </pre>
    </td>
  </tr>
</table>
</xsl:template>

<!-- Path -->
<xsl:template match="path">
<span class="path"><xsl:value-of select="."/></span>
</xsl:template>

<!-- Url -->
<xsl:template match="uri">
<!-- expand templates to handle things like <uri link="http://bar"><c>foo</c></uri> -->
<xsl:choose>
  <xsl:when test="@link">
    <xsl:choose>
      <xsl:when test="substring(@link,1,1) = '?'">
        <!-- We are dealing with a handbook link -->
        <xsl:choose>
          <!-- ?part=X&chap=Y, or ?chap=Y&part=X -->
          <xsl:when test="starts-with(@link, '?part=')">
            <xsl:variable name="thePart" select="substring(substring-before(@link, '&amp;'), 7, 20)"/>
            <xsl:variable name="theChap" select="substring(substring-after(@link, '&amp;'), 6, 20)"/>
            <a href="hb_part{$thePart}_chap{$theChap}.html"><xsl:apply-templates /></a>
          </xsl:when>
          <xsl:when test="starts-with(@link, '?chap=')">
            <xsl:variable name="thePart" select="substring(substring-after(@link, '&amp;'), 6, 20)"/>
            <xsl:variable name="theChap" select="substring(substring-before(@link, '&amp;'), 7, 20)"/>
            <a href="hb_part{$thePart}_chap{$theChap}.html"><xsl:apply-templates /></a>
          </xsl:when>
          <xsl:otherwise>
            <a href="{@link}"><xsl:apply-templates /></a>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="starts-with(@link, '/')">
        <a href="http://www.gentoo.org{@link}"><xsl:apply-templates /></a>
      </xsl:when>
      <xsl:otherwise>
        <a href="{@link}"><xsl:apply-templates/></a>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:when>
  <xsl:otherwise>
    <xsl:variable name="loc" select="."/>
    <a href="{$loc}"><xsl:apply-templates/></a>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<!-- Paragraph -->
<xsl:template match="p">
<xsl:param name="chid"/>
<xsl:choose>
  <xsl:when test="@class">
    <p class="{@class}">
      <xsl:apply-templates>
        <xsl:with-param name="chid" select="$chid"/>
      </xsl:apply-templates>
    </p>
  </xsl:when>
  <xsl:otherwise>
    <p>
      <xsl:apply-templates>
        <xsl:with-param name="chid" select="$chid"/>
      </xsl:apply-templates>
    </p>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<!-- Emphasize -->
<xsl:template match="e">
  <span class="emphasis"><xsl:apply-templates/></span>
</xsl:template>

<!-- E-mail address -->
<xsl:template match="mail">
<a href="mailto:{@link}"><xsl:value-of select="."/></a>
</xsl:template>

<!-- Table -->
<xsl:template match="table">
<table class="ntable">
  <xsl:apply-templates/>
</table>
</xsl:template>

<!-- Table Row -->
<xsl:template match="tr">
<tr>
  <xsl:apply-templates/>
</tr>
</xsl:template>

<!-- Table Item -->
<xsl:template match="ti">
<td bgcolor="#ddddff" class="tableinfo">
  <xsl:apply-templates/>
</td>
</xsl:template>

<!-- Table Heading -->
<xsl:template match="th">
<td bgcolor="#7a5ada" class="infohead">
  <b>
    <xsl:apply-templates/>
  </b>
</td>
</xsl:template>

<!-- Unnumbered List -->
<xsl:template match="ul">
<ul>
  <xsl:apply-templates/>
</ul>
</xsl:template>

<!-- Ordered List -->
<xsl:template match="ol">
<ol>
  <xsl:apply-templates/>
</ol>
</xsl:template>

<!-- List Item -->
<xsl:template match="li">
<li>
  <xsl:apply-templates/>
</li>
</xsl:template>

<!-- NOP -->
<xsl:template match="ignoreinemail">
<xsl:apply-templates/>
</xsl:template>

<!-- NOP -->
<xsl:template match="ignoreinguide">
</xsl:template>

<!-- License Tag -->
<xsl:template match="license">
<xsl:if test="$style != 'printable'">
<tt>
  The contents of this document are licensed under the <a href="http://creativecommons.org/licenses/by-sa/2.0">Creative Commons - Attribution / Share Alike</a> license.
</tt>
</xsl:if>

</xsl:template>

<!-- Define global variables; if a user has
     already defined those, this is a NOP -->
<xsl:param name="part">0</xsl:param>
<xsl:param name="chap">0</xsl:param>

<!-- A book -->
<xsl:template match="/book">
  <!-- If chap = 0, show an index -->
  <xsl:choose>
    <xsl:when test="$part != 0">
      <xsl:apply-templates select="part" />
    </xsl:when>
    <xsl:otherwise>
      <xsl:choose>
        <xsl:when test="$style = 'printable'">
          <xsl:call-template name="printdoclayout" />
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="doclayout"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- Content of /book -->
<xsl:template name="bookcontent">
  <xsl:call-template name="menubar" />
  <h1><xsl:value-of select="title" /></h1>
  <xsl:if test="$style = 'printable'">
    <xsl:apply-templates select="author" />
  </xsl:if>
  <p>Content:</p>
  <ul>
    <xsl:for-each select="part">
      <xsl:variable name="curpart" select="position()" />
      <li>
        <b><a href="hb_part{$curpart}_chap0.html"><xsl:value-of select="title" /></a></b>
        <xsl:if test="abstract">
          <br />
          <xsl:value-of select="abstract" />
        </xsl:if>
        <ol>
          <xsl:for-each select="chapter">
            <xsl:variable name="curchap" select="position()" />
            <li>
              <b><a href="hb_part{$curpart}_chap{$curchap}.html"><xsl:value-of select="title" /></a></b>
              <xsl:if test="abstract">
                <br/>
                <xsl:value-of select="abstract" />
              </xsl:if>
            </li>
          </xsl:for-each>
        </ol>
      </li>
    </xsl:for-each>
  </ul>
  <xsl:call-template name="menubar" />
  <xsl:apply-templates select="/book/license" />
</xsl:template>

<!-- Part inside a book -->
<xsl:template match="/book/part">
  <xsl:if test="($chap != 0) and ($part = position())">
    <xsl:apply-templates select="chapter" />
  </xsl:if>
  <xsl:if test="($chap = 0) and ($part = position())">
    <xsl:choose>
      <xsl:when test="$style = 'printable'">
        <xsl:call-template name="printdoclayout" />
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="doclayout" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>
</xsl:template>

<!-- Content of /book/part -->
<xsl:template name="bookpartcontent">
  <xsl:call-template name="menubar" />
  <xsl:if test="@id">
    <a name="{@id}"/>
  </xsl:if>
  <h1><xsl:number level="multiple" format="1. " value="position()"/><xsl:value-of select="title" /></h1>
  <xsl:if test="abstract">
    <p><xsl:value-of select="abstract" /></p>
  </xsl:if>
  <p>Content:</p>
  <ol>
    <xsl:for-each select="chapter">
      <xsl:variable name="curpos" select="position()" />
      <xsl:if test="title">
        <li>
          <b><a href="hb_part{$part}_chap{$curpos}.html"><xsl:value-of select="title" /></a></b>
          <xsl:if test="abstract">
            <br/><xsl:value-of select="abstract" />
          </xsl:if>
        </li>
      </xsl:if>
    </xsl:for-each>
  </ol>
  
  <xsl:call-template name="menubar" />
  <xsl:apply-templates select="/book/license" />
</xsl:template>

<!-- Menu bar -->
<xsl:template name="menubar">
  <xsl:variable name="prevpart" select="number($part) - 1" />
  <xsl:variable name="prevchap" select="number($chap) - 1" />
  <xsl:variable name="nextpart" select="number($part) + 1" />
  <xsl:variable name="nextchap" select="number($chap) + 1" />
    <hr />
    <p class="alttext">
      <!-- Previous Parts -->
      <xsl:choose>
        <xsl:when test="number($prevpart) &lt; 1">
          [ &lt;&lt; ]
        </xsl:when>
        <xsl:otherwise>
          [ <a href="hb_part{$prevpart}_chap0.html">&lt;&lt;</a> ]
        </xsl:otherwise>
      </xsl:choose>
      <!-- Previous Chapter -->
      <xsl:choose>
        <xsl:when test="number($prevchap) &lt; 1">
          [ &lt; ]
        </xsl:when>
        <xsl:otherwise>
          [ <a href="hb_part{$part}_chap{$prevchap}.html">&lt;</a> ]
        </xsl:otherwise>
      </xsl:choose>
      <!-- Content -->
      [ <a href="index.html">Home</a> ]
      <!-- Next Chapter -->
      <xsl:if test="name() = 'book'">
        [ <a href="hb_part1_chap0.html">&gt;</a> ]
      </xsl:if>
      <xsl:if test="name() = 'part'">
        [ <a href="hb_part{$part}_chap1.html">&gt;</a> ]
      </xsl:if>
      <xsl:if test="name() = 'chapter'">
        <xsl:choose>
          <xsl:when test="last() = position()">
            [ &gt; ]
          </xsl:when>
          <xsl:otherwise>
            [ <a href="hb_part{$part}_chap{$nextchap}.html">&gt;</a> ]
          </xsl:otherwise>
        </xsl:choose>
      </xsl:if>
      <!-- Next Part -->
      <xsl:if test="name() = 'book'">
        [ <a href="hb_part{$nextpart}_chap0.html">&gt;&gt;</a> ]
      </xsl:if>
      <xsl:if test="name() = 'part'">
        <xsl:choose>
          <xsl:when test="number($part) = last()">
            [ &gt;&gt; ]
          </xsl:when>
          <xsl:otherwise>
            [ <a href="hb_part{$nextpart}_chap0.html">&gt;&gt;</a> ]
          </xsl:otherwise>
        </xsl:choose>
      </xsl:if>
      <xsl:if test="name() = 'chapter'">
        <xsl:choose>
          <xsl:when test="count(/book/part) = number($part)">
            [ &gt;&gt; ] 
          </xsl:when>
          <xsl:otherwise>
            [ <a href="hb_part{$nextpart}_chap0.html">&gt;&gt;</a> ]
          </xsl:otherwise>
        </xsl:choose>
      </xsl:if>
    </p>
    <hr />
</xsl:template>


<!-- Chapter inside a part -->
<xsl:template match="/book/part/chapter">
  <xsl:if test="$chap = position()">
    <xsl:choose>
      <xsl:when test="$style = 'printable'">
        <xsl:call-template name="printdoclayout" />
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="doclayout" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>
</xsl:template>

<!-- Content of /book/part/chapter -->
<xsl:template name="bookpartchaptercontent">
  <xsl:call-template name="menubar" />
  <xsl:if test="@id">
    <a name="{@id}"/>
  </xsl:if>
  <h1><xsl:number level="multiple" format="1. " value="position()"/><xsl:value-of select="title" /></h1>
  <xsl:variable name="doc" select="include/@href"/>
  <xsl:variable name="FILE" select="document($doc)" />
  <xsl:if test="$FILE/sections/section/title">
    <b>Content: </b>
    <ul>
      <xsl:for-each select="$FILE/sections/section/title">
        <xsl:variable name="pos" select="position()" />
        <li><a href="#doc_chap{$pos}" class="altlink"><xsl:value-of select="." /></a></li>
      </xsl:for-each>
    </ul>
  </xsl:if>
  <xsl:apply-templates select="$FILE/sections/section" />
  
  <xsl:call-template name="menubar" />
  <xsl:apply-templates select="/book/license" />
</xsl:template>

<!-- Section inside a chapter -->
<xsl:template match="/sections/section">
  <xsl:param name="pos" select="position()" />
  <a name="doc_chap{$pos}"/>
  <xsl:if test="@id">
    <a name="{@id}"/>
  </xsl:if>
  <xsl:if test="title">
    <p class="chaphead"><span class="chapnum"><xsl:value-of select="$chap" />.<xsl:number level="multiple" format="a. " value="position()" /></span><xsl:value-of select="title" /></p>
  </xsl:if>
  <xsl:apply-templates select="body|subsection">
    <xsl:with-param name="chpos" select="$pos"/>
  </xsl:apply-templates>
</xsl:template>

<!-- Subsubsection inside a section -->
<xsl:template match="/sections/section/subsection">
  <xsl:param name="pos" select="position()"/>
  <xsl:param name="chpos" />
  <a name="doc_chap{$chpos}_sect{$pos}" />
  <xsl:if test="@id">
    <a name="{@id}"/>
  </xsl:if>
  <p class="secthead"><xsl:value-of select="title" /></p>
  <xsl:apply-templates select="body" />
</xsl:template>

<xsl:template name="content">
  <xsl:if test="local-name() = 'guide'">
    <!-- Inside /guide -->
    <xsl:call-template name="guidecontent" />
  </xsl:if>
  <xsl:if test="local-name() = 'book'">
    <!-- Inside /book -->
    <xsl:call-template name="bookcontent" />
  </xsl:if>
  <xsl:if test="local-name() = 'part'">
    <!-- Inside /book/part -->
    <xsl:call-template name="bookpartcontent" />
  </xsl:if>
  <xsl:if test="local-name() = 'chapter'">
    <!-- Inside /book/part/chapter -->
    <xsl:call-template name="bookpartchaptercontent" />
  </xsl:if>
</xsl:template>

<!-- Printable style for /guide -->
<xsl:template name="printdoclayout">
<html>
<head>
  <link title="new" rel="stylesheet" href="css/main.css" type="text/css"/>
  <title>
    Printable Linux 
    <xsl:choose>
      <xsl:when test="/guide/@type='project'">Projects</xsl:when>
      <xsl:otherwise>Documentation</xsl:otherwise>
    </xsl:choose>
    -- 
    <xsl:choose>
      <xsl:when test="subtitle"><xsl:value-of select="title"/>: <xsl:value-of select="subtitle"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="title"/></xsl:otherwise>
    </xsl:choose>
  </title>
</head>
<body style="margin-right: 35mm;" bgcolor="#ffffff">

<!-- Content goes here -->
<xsl:call-template name="content" />

</body>
</html>
</xsl:template>

</xsl:stylesheet>
