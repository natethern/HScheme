<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
        "http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">
<HTML>
<HEAD>
	<TITLE>HScheme Issues</TITLE>
</HEAD>
<BODY>

<H1>HScheme Issues</H1>
<P>HScheme is intended to be R<SUP>5</SUP>RS-compliant when run with the default monad and bindings. As of <? echo date("Y-m-d H:i",getlastmod()); ?>, these issues stand before release:</P>
<UL>
<LI><P>Macros are not hygienic. I just noticed this after adding test cases from chapter 4 of R<SUP>5</SUP>RS. This is my current top priority.</P>
<LI><P>Ellipses don't work in vectors in <TT>syntax-rules</TT>.</P>
<LI><P>The <TT>quasiquote</TT> implementation isn't standard.</P>
<LI><P>The procedure <TT>eval</TT> exists, but no environments are available to use it with.</P>
<LI><P>I expect there are missing and broken procedures and macros. I haven't yet checked every one.</P>
<LI><P>Also, the parser is very slow, many times slower than it needs to be.</P>
</UL>
<P CLASS=path>
<A HREF="./">HScheme</A> -&gt;
<SPAN CLASS=here>Issues</SPAN>
<P>
<A HREF="http://sourceforge.net"><IMG SRC="http://sourceforge.net/sflogo.php?group_id=47823" ALT="SourceForge"></A>
</P>
</BODY>
</HTML>
