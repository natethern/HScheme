<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
        "http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">
<HTML>
<HEAD>
	<TITLE>Interpret HScheme</TITLE>
</HEAD>
<BODY>
<P>
<?
if (file_exists("../cgi-bin/interpret.cgi"))
	{
	echo "Interpreter last modified ".
	 date("Y-m-d H:i.",filemtime("../cgi-bin/interpret.cgi"));
	}
else echo "<B>Interpreter not available.</B>"
?>
</P>
<FORM ACTION="../cgi-bin/interpret.cgi" METHOD="post">
<P>Scheme Code:<BR>
<TEXTAREA NAME="input" ROWS="40" COLS="100">
</TEXTAREA><BR>
<INPUT TYPE="RADIO" NAME="monad" VALUE="gcps" CHECKED> full
<INPUT TYPE="RADIO" NAME="monad" VALUE="pure"> pure
<INPUT TYPE="RESET" VALUE="Clear">
<INPUT TYPE="SUBMIT" VALUE="Interpret">
</FORM>
<P>"Pure" means purely functional procedures only.</P>
<P CLASS=path>
<A HREF="./">HScheme</A> -&gt;
<SPAN CLASS=here>Interpret</SPAN>
<P>
<A HREF="http://sourceforge.net"><IMG SRC="http://sourceforge.net/sflogo.php?group_id=47823" ALT="SourceForge"></A>
</P>
</BODY>
</HTML>
