<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template name="header-information">
	<xsl:param name="matchid"/>

	<!-- <style type="text/css" media="all">@import 
		"/style/ggp.css";</style>
	-->

	<style>

	div.trace {
	   /* border: 1px solid black; */
	}
	div.subtrace {
	   /* border: 1px solid red; */
	   margin-left: 0px;
	}
	div.sentence {
	   position: relative;
	   clear: left;
	}
	div.head {
	   position: relative;
	   float: left;
	   margin-left: 4px;
	   width: 23px;
	}
	div.body {
	   position: relative;
	   float: left;
	   margin-left: 2px;
	   margin-top: 2px;
	}
	span.white {
	   color: white;
	}
	</style>
	<script language="JavaScript">
	
	function showDetails(divID) {
	   var strShow
		   strShow = divID+"Show";
	   var strHide
		   strHide = divID+"Hide";
	
	   if (document.getElementById)  /* Netscape */
	   {
		   document.getElementById(divID).style.display = "block";
		   document.getElementById(strShow).style.display = "none";
		   document.getElementById(strHide).style.display = "block";
	   }
	   else if (document.all)  /* IE */
	   {
		   document.all[divID].style.display = "block";
		   document.all[strShow].style.display = "none";
		   document.all[strHide].style.display = "block";
	   }
	}
	
	function hideDetails(divID) {
	   var strShow
		   strShow = divID+"Show";
	   var strHide
		   strHide = divID+"Hide";
	
	   if (document.getElementById)  /* Netscape */
	   {
		   document.getElementById(divID).style.display = "none";
		   document.getElementById(strShow).style.display = "block";
		   document.getElementById(strHide).style.display = "none";
	   }
	   else if (document.all)  /* IE */
	   {
		   document.all[divID].style.display = "none";
		   document.all[strShow].style.display = "block";
		   document.all[strHide].style.display = "none";
	   }
	}
	
	</script>

	<SCRIPT language="JavaScript">
	function move() {
	 var manual="debugwalk?kind=manual%26matchid=<xsl:value-of select="$matchid"/>%26moves=";
	 var moves="(";
	 <xsl:for-each select="role">
		moves += escape(window.document.form1.<xsl:value-of select="concat(.,'move')"/>.value);
		moves += escape(" ");
	 </xsl:for-each>
	 moves += ")";
	 manual += moves;
     location.href = manual;	
	}
	function specialwalk(type) {
		location.href="debugwalk?kind="+type+"%26matchid=<xsl:value-of select="$matchid"/>";
	}
	function resetgame() {
		location.href="debugreset?matchid=<xsl:value-of select="$matchid"/>";
	}
	function backup(step) {
		location.href="debugbackup?step="+step+"%26matchid=<xsl:value-of select="$matchid"/>";
	}
	function upload() {
		location.href="uploadaxioms?matchid=<xsl:value-of select="$matchid"/>";
	}
	function standardgame() {
		location.href="standardgame?matchid=<xsl:value-of select="$matchid"/>";
	}
	
	var processing = false;
	function processmove(p,x,y) {
		if (window.document.form1.explainer.checked == true) {
			window.document.form1.fact.value = "(cell "+x+" "+y+" "+p+")";
			explain();
		} else {
			if (window.document.form1.control.value == 'BLACK') {	
				if (!processing) {	
					window.document.form1.blackmove.value += "(move "+p+" "+x+" "+y+" ";
					processing = true;
				} else {
					window.document.form1.blackmove.value += x +" "+y+")";
					move();
				}
			} else { //White's move
				if (!processing) {	
					window.document.form1.whitemove.value += "(move "+p+" "+x+" "+y+" ";
					processing = true;
				} else {
					window.document.form1.whitemove.value += x +" "+y+")";
					move();
				}
			}
		}
	}
	function explain() {
		var command = "explain?matchid=<xsl:value-of select="$matchid"/>%26fact="
		command += escape(window.document.form1.fact.value);
     	location.href = command;	
		}
	</SCRIPT>
</xsl:template>

<xsl:template name="body-top">
<!-- <xsl:attribute name="onLoad">Down(0,<xsl:value-of select="match/playclock"/>)</xsl:attribute>
-->
<div id="underline"></div>
<br/>
<xsl:if test="contains(match/match-id,'Rerun')">
	<div class="nextbutton">
		<a><xsl:attribute name="href">stepmatch?Match=
				<xsl:value-of select="match/match-id"/>
			</xsl:attribute>
			Next</a>	
	</div>
</xsl:if>
<br/>
</xsl:template>

<xsl:template name="form">
<FORM ACTION="debugwalk?" METHOD="POST" NAME="form1">
<div id="movebox">
<a href="javascript:specialwalk('random')">Random Move</a> | 
<a href="javascript:specialwalk('legal')">Legal Move</a> | 
<a href="javascript:specialwalk('goals')">Goals</a> |
<a href="javascript:specialwalk('terminal')">Terminal</a> |
<a href="javascript:specialwalk('freeze')">Reload State</a> 
<br/>
<a href="javascript:resetgame()">Reset game</a> | 
<a href="javascript:upload()">Upload new rules</a>
<br/>Make Moves:
<xsl:for-each select="role">
	<br/>
	<xsl:value-of select="."/>'s Move:
	<INPUT TYPE="TEXT" SIZE="40">
		<xsl:attribute name="ID"><xsl:value-of select="."/>move</xsl:attribute>
	</INPUT>
</xsl:for-each>
<INPUT TYPE="BUTTON" VALUE="Make moves" onClick="move()"/>
<br/>Explain Fact: <INPUT TYPE="TEXT" ID="fact" SIZE="40"></INPUT>
<INPUT TYPE="BUTTON" VALUE="Explain?" onClick="explain()"/>
</div>
</FORM>
<br/>
</xsl:template>

<!-- History -->
<xsl:template match="history">
<div id="history">
<table border="1">
    <xsl:for-each select="step">
    <xsl:sort select="step-number" data-type="number"/>
	    <tr>
    		<td><a><xsl:attribute name="href">
    					<xsl:choose>
						<xsl:when test="stepnumber = /match/history/step[last()]/step-number">
							javascript:backup(0)
						</xsl:when>
						<xsl:otherwise>
							javascript:backup(<xsl:value-of select="step-number"/>) </xsl:otherwise>
						</xsl:choose>
				</xsl:attribute><xsl:value-of select="step-number"/></a></td>
      		<xsl:for-each select="move">
				<td><xsl:value-of select="."/></td>
			</xsl:for-each>
	    </tr>
    </xsl:for-each>
</table>
</div>
</xsl:template>

<!--   Tracing -->

<xsl:template match="tracewrap">
	<div class="trace">
	<xsl:apply-templates select="trace" />
	</div>
</xsl:template>

<xsl:template match="trace">
	<xsl:variable name="handle"> <xsl:value-of select="subtrace/@id"/></xsl:variable>
	<div class="sentence">
	<div class="head">
	<xsl:choose>
	<xsl:when test="count(subtrace/trace) > 0">
	<a href="javascript:showDetails('{$handle}')">
<img src="http://games.stanford.edu/gamemaster/images/arrowrt.gif" width="21" height="19" id="{$handle}Show" style="display:block;" border="0" /> </a>
	<a href="javascript:hideDetails('{$handle}')">
<img src="http://games.stanford.edu/gamemaster/images/arrowdn.gif" width="21" height="19" id="{$handle}Hide" style="display:none" border="0" /></a>
	
	</xsl:when>
	<xsl:otherwise>
	<span class="white">.</span>
	</xsl:otherwise>
	</xsl:choose>
	</div>
	<div class="body">
	<xsl:apply-templates select="open"/>
	<xsl:apply-templates select="subtrace" />
	<xsl:apply-templates select="close"/>
	</div>	
	</div>
</xsl:template>



<xsl:template match="subtrace">
	<xsl:variable name="handle"> <xsl:value-of select="@id"/></xsl:variable>
	<div class="subtrace" id="{$handle}" style="display:none">
	<xsl:apply-templates /></div>
</xsl:template>

<xsl:template match="open">
	<div class="sentence">
	<span class="open"><xsl:value-of select="." /></span>
	</div>
</xsl:template>
<xsl:template match="close">
	<div class="sentence">
	<span class="open"><xsl:value-of select="." /></span>
	</div>
</xsl:template>


</xsl:stylesheet>