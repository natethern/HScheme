<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
        "http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">
<HTML>
<HEAD>
	<TITLE>HScheme Examples</TITLE>
</HEAD>
<BODY>

<H1>HScheme Examples</H1>
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
<H2>1000 Factorial</H2>
<FORM ACTION="../cgi-bin/interpret.cgi" METHOD="post">
<TEXTAREA NAME="input" ROWS="7" COLS="80">
(define factorial (lambda (x)
  (if (= x 0) 1 (* x (factorial (- x 1))))
))

(factorial 1000)
</TEXTAREA><BR>
<INPUT TYPE="RADIO" NAME="monad" VALUE="gcps" CHECKED> full
<INPUT TYPE="RADIO" NAME="monad" VALUE="pure"> pure
<INPUT TYPE="RESET" VALUE="Revert">
<INPUT TYPE="SUBMIT" VALUE="Interpret">
</FORM>

<H2>Fixed Point</H2>
<FORM ACTION="../cgi-bin/interpret.cgi" METHOD="post">
<TEXTAREA NAME="input" ROWS="7" COLS="80">
(define hello-goodbye-etc (lambda (x) `(hello goodbye . ,x) ))
(hello-goodbye-etc '(a b c))
(define hello-goodbyes (call-with-result hello-goodbye-etc))
(list-head hello-goodbyes 30)
</TEXTAREA><BR>
<INPUT TYPE="RADIO" NAME="monad" VALUE="gcps" CHECKED> full
<INPUT TYPE="RADIO" NAME="monad" VALUE="pure"> pure
<INPUT TYPE="RESET" VALUE="Revert">
<INPUT TYPE="SUBMIT" VALUE="Interpret">
</FORM>

<P>"Pure" means purely functional procedures only.</P>
<P CLASS=path>
<A HREF="./">HScheme</A> -&gt;
<SPAN CLASS=here>Examples</SPAN>
<P>
<A HREF="http://sourceforge.net"><IMG SRC="http://sourceforge.net/sflogo.php?group_id=47823" ALT="SourceForge"></A>
</P>
</BODY>
</HTML>
