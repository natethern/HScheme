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
<LI><P>The procedure <TT>eval</TT> exists, but no environments are available to use it with.
<LI><P>Currently procedures always compare false (e.g., <TT>(eqv? car car)</TT>).</P>
<LI><P>HScheme returns exact results to certain functions, such as <TT>floor</TT>, <TT>quotient</TT>, <TT>rationalize</TT>, even when given inexact arguments. Also, an inexact finite number multiplied by exact zero, etc., is exact zero. Frankly I think that's correct, but R5RS says otherwise.</P>
<LI><P>The procedure <TT>list?</TT> doesn't terminate when given a circular "list".</P>
<LI><P>Ellipses currently don't work in syntax defintions.</P>
<LI><P>I expect there are missing procedures and macros. I haven't yet checked every one.</P>
</UL>
<P CLASS=path>
<A HREF="./">HScheme</A> -&gt;
<SPAN CLASS=here>Issues</SPAN>
<P>
<A HREF="http://sourceforge.net"><IMG SRC="http://sourceforge.net/sflogo.php?group_id=47823" ALT="SourceForge"></A>
</P>
</BODY>
</HTML>
