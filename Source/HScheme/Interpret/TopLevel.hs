-- This is written in Haskell.
{--
HScheme -- a Scheme interpreter written in Haskell
Copyright (C) 2002 Ashley Yakeley <ashley@semantic.org>

This file is part of HScheme.

HScheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

HScheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HScheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--}

module Org.Org.Semantic.HScheme.Interpret.TopLevel
	(
	TopLevelCommand(..),TopLevelObjectCommand,TopLevelMacro(..),
	lambdaM,letSequentialM,letSeparateM,letRecursiveM,
	assembleTopLevelExpression,assembleTopLevelExpressionsEat,
	pureDefineT,fullDefineT,
	beginM,bodyM,bodyListM,
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Interpret.SymbolExpression;
	import Org.Org.Semantic.HScheme.Interpret.FunctorLambda;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	-- deep Haskell magic
	data ThingySingle m f obj a = forall b. MkThingySingle
	  ((b -> m obj) -> obj -> (a -> m obj))
	  (f (a -> m obj) -> f (b -> m obj));

	getThingySingle :: 
		(
		Build cm r,
		Scheme m r
		) =>
	 Object r m -> cm (ThingySingle m (SchemeExpression r m) (Object r m) a);
	-- b = a
	getThingySingle NilObject = return (MkThingySingle (\f obj a -> do
		{
		mobj <- getMaybeConvert obj;
		case mobj of
			{
			Nothing -> fail "too many arguments";
			Just (_ :: ()) -> f a;
			};
		}) id);
	-- b = p2 (p1 a)
	getThingySingle (PairObject l1 l2) = do
		{
		o1 <- get l1;
		o2 <- get l2;
		MkThingySingle map1 bindSyms1 <- getThingySingle o1;
		MkThingySingle map2 bindSyms2 <- getThingySingle o2;
		return (MkThingySingle (\f obj a -> do
			{
			mobj <- getMaybeConvert obj;
			case mobj of
				{
				Nothing -> fail "too few arguments";
				Just (p1,p2) -> (map1 (map2 f p2) p1) a;
				};
			})
		 (bindSyms2 . bindSyms1)
		 );
		};
	-- b = (mloc,a)
	getThingySingle (SymbolObject sym) = return (MkThingySingle (\bf obj a -> bf (new obj,a))
	 (\ps -> fmap (\f (mloc,a) -> f mloc a) (fAbstract sym ps)));
	getThingySingle _ = fail "bad arg list in lambda";

	data ThingyList m f obj a = forall b. MkThingyList
	  ((b -> m obj) -> [obj] -> (a -> m obj))
	  (f (a -> m obj) -> f (b -> m obj));

	getThingyList :: 
		(
		Build cm r,
		Scheme m r
		) =>
	 Object r m -> cm (ThingyList m (SchemeExpression r m) (Object r m) a);
	-- b = a
	getThingyList NilObject = return (MkThingyList (\f list a -> case list of
		{
		[] -> f a;
		_ -> fail "too many arguments";
		}) id);
	-- b = p2 (p1 a)
	getThingyList (PairObject l1 lr) = do
		{
		obj1 <- get l1;
		objr <- get lr;
		MkThingySingle map1 bindSyms1 <- getThingySingle obj1;
		MkThingyList map2 bindSyms2 <- getThingyList objr;
		return (MkThingyList (\f list a -> case list of
			{
			[] -> fail "too few arguments";
			(p1:p2) -> (map1 (map2 f p2) p1) a;
			})
		(bindSyms2 . bindSyms1));
		};
	-- b = (mloc,a)
	getThingyList (SymbolObject sym) = return (MkThingyList (\bf list a -> bf (do
		{
		obj <- getConvert list;
		new obj;
		},a))
	 (\ps -> fmap (\f (mloc,a) -> f mloc a) (fAbstract sym ps)));
	getThingyList _ = fail "bad arg list in lambda";

	makeLambda ::
		(
		Build cm r,
		Scheme m r
		) =>
	 Object r m ->
	 SchemeExpression r m (m (Object r m)) ->
	 cm (SchemeExpression r m ([Object r m] -> m (Object r m)));
	makeLambda params expr = do
		{
		MkThingyList map bindSyms <- getThingyList params;
		return (fmap ((\ff list -> ff list ()) . map) (bindSyms (fmap (\f () -> f) expr)));
		};

	convertToLocationExpression :: (Scheme m r) =>
	 SchemeExpression r m (m (Object r m)) ->
	 SchemeExpression r m (m (ObjLocation r m));
	convertToLocationExpression = fmap (\mvalue -> (do
	 	{
	 	value <- mvalue;
	 	new value;
	 	}));

	convertToLocationBinding :: (Scheme m r) =>
	 (Symbol,SchemeExpression r m (m (Object r m))) ->
	 (Symbol,SchemeExpression r m (m (ObjLocation r m)));
	convertToLocationBinding (sym,valueExpr) = (sym,convertToLocationExpression valueExpr);

	data TopLevelCommand cm r m a = MkTopLevelExpression
		{
		tleExpression :: SchemeExpression r m a,
		tleInitialBindings :: [(Symbol,SchemeExpression r m (m (Object r m)))],
		tleSyntaxes :: [(Symbol,Syntax cm r m)]
		};

	type TopLevelObjectCommand cm r m = TopLevelCommand cm r m (m (Object r m));

	newtype TopLevelMacro cm r m = MkTopLevelMacro ((
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 [Object r m] -> cm (TopLevelObjectCommand cm r m));

	lambdaM ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 (Object r m,[Object r m]) ->
	 cm (SchemeExpression r m (m (Object r m)));
	lambdaM (params,bodyObj) = do
		{
		body <- bodyM bodyObj;
		proc <- makeLambda params body;
		return (fmap (return . ProcedureObject) proc);
		};

	compileBinds ::
		(
		Build cm r,
		Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 [(Symbol,(Object r m,()))] ->
	 cm [(Symbol,SchemeExpression r m (m (Object r m)))];
	compileBinds [] = return [];
	compileBinds ((sym,(obj,())):bs) = do
		{
		expr <- assembleExpression obj;
		bcs <- compileBinds bs;
		return ((sym,expr):bcs);
		};

	substMap :: (Scheme m r) =>
	 (m (ObjLocation r m) -> m a) -> m (Object r m) -> m a;
	substMap va mvalue = do
		{
	 	value <- mvalue;
		loc <- new value;
		va (return loc);
		};

	letSequentialM ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (SchemeExpression r m (m (Object r m)));
	letSequentialM (bindList,bodyObj) = do
		{
		binds <- compileBinds bindList;
		body <- bodyM bodyObj;
		return (fSubstMapSequential substMap binds body);
		};

	letSeparateM ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (SchemeExpression r m (m (Object r m)));
	letSeparateM (bindList,bodyObj) = do
		{
		binds <- compileBinds bindList;
		body <- bodyM bodyObj;
		return (fSubstMapSeparate substMap binds body);
		};

	letRecursiveM ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (SchemeExpression r m (m (Object r m)));
	letRecursiveM (bindList,bodyObj) = do
		{
		binds <- compileBinds bindList;
		body <- bodyM bodyObj;
		return (fSubstMapRecursive substMap binds body);
		};

	assembleTopLevelObjectCommand ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 Object r m -> cm (TopLevelObjectCommand cm r m);
	assembleTopLevelObjectCommand obj = do
		{
		mpair <- getMaybeConvert obj;
		case mpair of
			{
			Just (sym,args) -> case getBinding ?toplevelbindings sym of
				{
				Just (MkTopLevelMacro tlm) -> tlm args;
				Nothing -> compileExprTopLevel;
				};
			Nothing -> compileExprTopLevel;
			};
		} where
		{
		compileExprTopLevel = do
			{
			expr <- assembleExpression obj;
			return (MkTopLevelExpression expr [] []);
			};
		};

	assembleTopLevelExpression ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 Object r m -> cm (SchemeExpression r m (m (Object r m)));
	assembleTopLevelExpression obj = do
		{
		MkTopLevelExpression expr _ _ <- assembleTopLevelObjectCommand obj;
		return expr;
		};

	-- 5.2 Definitions
	pureDefineT ::
		(
		Build cm r,
		Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (TopLevelObjectCommand cm r m);
	pureDefineT (sym,(val,())) = do
		{
		valExpr <- assembleExpression val;
		return (MkTopLevelExpression (return' (return nullObject)) [(sym,valExpr)] []);
		};

	fullDefineT ::
		(
		Build cm r,
		FullScheme m r,
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (TopLevelObjectCommand cm r m);
	fullDefineT (sym,(val,())) = do
		{
		valExpr <- assembleExpression val;
		return (MkTopLevelExpression 
		 (liftF2 (\mloc mval -> do
		 	{
		 	loc <- mloc;
		 	val <- mval;
		 	set loc val;
		 	return nullObject;
		 	}) (fSymbol sym) valExpr)
		 [(sym,return' (return (error "unassigned symbol")))] []);
		};

	begin ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 a -> 
	 (m (Object r m) -> a -> a) ->
	 [Object r m] ->
	 cm (TopLevelCommand cm r m a);
	begin none conn [] = return (MkTopLevelExpression (return' none) [] []);
	begin none conn (obj:objs) = do
		{
		(MkTopLevelExpression expr1 binds1 syntax1) <- assembleTopLevelObjectCommand obj;
		(MkTopLevelExpression exprr bindsr syntaxr) <- let
		 {?syntacticbindings = newBinds ?syntacticbindings syntax1} in
		 begin none conn objs;
		return (MkTopLevelExpression (liftF2 conn expr1 exprr) (binds1 ++ bindsr) (syntax1 ++ syntaxr));
		};

	beginM ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (TopLevelObjectCommand cm r m);
	beginM = begin (return nullObject) (>>);

	assembleTopLevelExpressions ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 m a -> 
	 (m (Object r m) -> m a -> m a) ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m a));
	assembleTopLevelExpressions none conn objs = do
		{
		MkTopLevelExpression expr binds _ <- begin none conn objs;
--		return (fSubstMapRecursive substMap binds expr);
		return (fSubstMapSequential substMap binds expr);
		};

	bodyM ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (SchemeExpression r m (m (Object r m)));
	bodyM = assembleTopLevelExpressions (return nullObject) (>>);

	assembleTopLevelExpressionsList ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (SchemeExpression r m (m [Object r m]));
	assembleTopLevelExpressionsList = assembleTopLevelExpressions
	 (return [])
	 (\mr mrs -> do
		{
		r <- mr;
		rs <- mrs;
		return (r:rs);
		});

	fmap' :: (Monad f) => (a -> b) -> (f a -> f b);
	fmap' map fa = fa >>= (return . map);

	bodyListM ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (SchemeExpression r m (m (Object r m)));
	bodyListM obj = fmap' (fmap (\mlist -> mlist >>= getConvert)) (assembleTopLevelExpressionsList obj);

	assembleTopLevelExpressionsEat ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 (Object r m -> m ()) ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m ()));
	assembleTopLevelExpressionsEat eat = assembleTopLevelExpressions
	 (return ())
	 (\mr mrs -> do
		{
		r <- mr;
		eat r;
		mrs;
		});

{--
top-level: define, load, define-syntax
macro: quote, lambda, if, let, let*, syntax-rules, case-match, letrec, begin, set!


(let ((a b))
	(let ((f (lambda (x) (list x a))))
		(f obj)
	)
)

(define f (lambda () a))
(let ((a 'b))
 (f)
)
--}
	}
