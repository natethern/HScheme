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
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 Object r m -> cm (ThingySingle m (SchemeExpression r m) (Object r m) a);
	-- b = a
	getThingySingle NilObject = return (MkThingySingle (\f obj a -> do
		{
		mobj <- getMaybeConvert obj;
		case mobj of
			{
			Nothing -> throwArgError "extra-arguments" [obj];
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
				Nothing -> throwArgError "missing-arguments" [];
				Just (p1,p2) -> (map1 (map2 f p2) p1) a;
				};
			})
		 (bindSyms2 . bindSyms1)
		 );
		};
	-- b = (loc,a)
	getThingySingle (SymbolObject sym) = return (MkThingySingle (\bf obj a -> do
		{
		loc <- new obj;
		bf (loc,a)
		})
	 (\ps -> fmap (\f (loc,a) -> f loc a) (fAbstract sym ps)));
	getThingySingle obj = throwArgError "bad-lambda-arg-list" [obj];

	data ThingyList m f obj a = forall b. MkThingyList
	  ((b -> m obj) -> [obj] -> (a -> m obj))
	  (f (a -> m obj) -> f (b -> m obj));

	getThingyList :: 
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m),
		Scheme m r
		) =>
	 Object r m -> cm (ThingyList m (SchemeExpression r m) (Object r m) a);
	-- b = a
	getThingyList NilObject = return (MkThingyList (\f list a -> case list of
		{
		[] -> f a;
		(obj:_) -> throwArgError "extra-arguments" [obj];
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
			[] -> throwArgError "missing-arguments" [];
			(p1:p2) -> (map1 (map2 f p2) p1) a;
			})
		(bindSyms2 . bindSyms1));
		};
	-- b = (loc,a)
	getThingyList (SymbolObject sym) = return (MkThingyList (\bf list a -> do
		{
		obj <- getConvert list;
		loc <- new obj;
		bf (loc,a);
		})
	 (\ps -> fmap (\f (loc,a) -> f loc a) (fAbstract sym ps)));
	getThingyList obj = throwArgError "bad-lambda-arg-list" [obj];

	makeLambda ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 Object r m ->
	 ObjectSchemeExpression r m ->
	 cm (SchemeExpression r m ([Object r m] -> m (Object r m)));
	makeLambda params expr = do
		{
		MkThingyList map bindSyms <- getThingyList params;
		return (fmap ((\ff list -> ff list ()) . map) (bindSyms (fmap (\f () -> f) expr)));
		};

	data TopLevelCommand r m a = MkTopLevelCommand
		{
		tleExpression :: SchemeExpression r m a,
		tleInitialBindings :: [(Symbol,ObjectSchemeExpression r m)],
		tleSyntaxes :: [(Symbol,Syntax r (Object r m))]
		};

	type TopLevelObjectCommand r m = TopLevelCommand r m (m (Object r m));

	newtype TopLevelMacro cm r m = MkTopLevelMacro ((
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] -> cm (TopLevelObjectCommand r m));

	compileBinds ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [(Symbol,(Object r m,()))] ->
	 cm [(Symbol,ObjectSchemeExpression r m)];
	compileBinds [] = return [];
	compileBinds ((sym,(obj,())):bs) = do
		{
		expr <- assembleExpression obj;
		bcs <- compileBinds bs;
		return ((sym,expr):bcs);
		};

	substMap :: (Scheme m r) =>
	 (ObjLocation r m -> m a) -> m (Object r m) -> m a;
	substMap va mvalue = do
		{
	 	value <- mvalue;
		loc <- new value;
		va loc;
		};

	letSequentialM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (ObjectSchemeExpression r m);
	letSequentialM (bindList,bodyObj) = do
		{
		binds <- compileBinds bindList;
		body <- bodyM bodyObj;
		return (fSubstMapSequential substMap binds body);
		};

	letSeparateM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (ObjectSchemeExpression r m);
	letSeparateM (bindList,bodyObj) = do
		{
		binds <- compileBinds bindList;
		body <- bodyM bodyObj;
		return (fSubstMapSeparate substMap binds body);
		};

--	fixFunctor :: (Functor t) => t (t a -> a) -> t a;
--	fixFunctor t = fix (\p -> (fmap (\x -> x p) t));

	mfixExFunctor :: (MonadFix m,FunctorApplyReturn m,ExtractableFunctor t) =>
	 t (t a -> m a) -> m (t a);
	mfixExFunctor t = mfix (\p -> fExtract (fmap (\x -> x p) t));

	fixer :: (MonadFix m,FunctorApplyReturn m,ExtractableFunctor t,Scheme m r) =>
	 t (t (ObjLocation r m) -> m (Object r m)) ->
	 (t (ObjLocation r m) -> m (Object r m)) ->
	 m (Object r m);
	fixer bindsT bodyT = (mfixExFunctor (fmap (\tlocmobj tloc -> do
		{
		obj <- tlocmobj tloc;
		new obj;
		}) bindsT)) >>= bodyT;

	letRecursiveM ::
		(
		MonadFix m, FunctorApplyReturn m,
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (ObjectSchemeExpression r m);
	letRecursiveM (bindList,bodyObj) = do
		{
		binds <- compileBinds bindList;
		body <- bodyM bodyObj;
		return (fSubstMapRecursive fixer binds body);
		};

	assembleTopLevelObjectCommand ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 Object r m -> cm (TopLevelObjectCommand r m);
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
			return (MkTopLevelCommand expr [] []);
			};
		};

	assembleTopLevelExpression ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 Object r m -> cm (ObjectSchemeExpression r m);
	assembleTopLevelExpression obj = do
		{
		MkTopLevelCommand expr _ _ <- assembleTopLevelObjectCommand obj;
		return expr;
		};

	-- 5.2 Definitions
	pureDefineT ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (TopLevelObjectCommand r m);
	pureDefineT (sym,(val,())) = do
		{
		valExpr <- assembleExpression val;
		return (MkTopLevelCommand (return' (return nullObject)) [(sym,valExpr)] []);
		};

	fullDefineT ::
		(
		BuildThrow cm (Object r m) r,
		FullScheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (TopLevelObjectCommand r m);
	fullDefineT (sym,(val,())) = do
		{
		valExpr <- assembleExpression val;
		return (MkTopLevelCommand 
		 (liftF2 (\loc mval -> do
		 	{
		 	val <- mval;
		 	set loc val;
		 	return nullObject;
		 	}) (fSymbol sym) valExpr)
		 [(sym,return' (return (error "unassigned symbol")))] []);
		};

	begin ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 a -> 
	 (m (Object r m) -> a) ->
	 (m (Object r m) -> a -> a) ->
	 [Object r m] ->
	 cm (TopLevelCommand r m a);
	begin none one conn [] = return (MkTopLevelCommand (return' none) [] []);
	begin none one conn [obj] = do
		{
		(MkTopLevelCommand expr binds syntax) <- assembleTopLevelObjectCommand obj;
		return (MkTopLevelCommand (fmap one expr) binds syntax);
		};
	begin none one conn (obj:objs) = do
		{
		(MkTopLevelCommand expr1 binds1 syntax1) <- assembleTopLevelObjectCommand obj;
		(MkTopLevelCommand exprr bindsr syntaxr) <- let
		 {?syntacticbindings = newBindings ?syntacticbindings syntax1} in
		 begin none one conn objs;
		return (MkTopLevelCommand (liftF2 conn expr1 exprr) (binds1 ++ bindsr) (syntax1 ++ syntaxr));
		};

	beginM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (TopLevelObjectCommand r m);
	beginM = begin (return nullObject) id (>>);

	assembleTopLevelExpressions ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 m a -> 
	 (m (Object r m) -> m a) ->
	 (m (Object r m) -> m a -> m a) ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m a));
	assembleTopLevelExpressions none one conn objs = do
		{
		MkTopLevelCommand expr binds _ <- begin none one conn objs;
--		return (fSubstMapRecursive substMap binds expr);
		return (fSubstMapSequential substMap binds expr);
		};

	bodyM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (ObjectSchemeExpression r m);
	bodyM = assembleTopLevelExpressions (return nullObject) id (>>);

	lambdaM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Object r m,[Object r m]) ->
	 cm (ObjectSchemeExpression r m);
	lambdaM (params,bodyObj) = do
		{
		body <- bodyM bodyObj;
		proc <- makeLambda params body;
		return (fmap (return . ProcedureObject) proc);
		};

	assembleTopLevelExpressionsList ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (SchemeExpression r m (m [Object r m]));
	assembleTopLevelExpressionsList = assembleTopLevelExpressions
	 (return [])
	 (\mr -> do
	 	{
		r <- mr;
		return [r];
	 	})
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
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (ObjectSchemeExpression r m);
	bodyListM obj = fmap' (fmap (\mlist -> mlist >>= getConvert)) (assembleTopLevelExpressionsList obj);

	assembleTopLevelExpressionsEat ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Object r m -> m ()) ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m ()));
	assembleTopLevelExpressionsEat eat = assembleTopLevelExpressions
	 (return ())
	 (\mr -> do
	 	{
		r <- mr;
		eat r;
	 	})
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
