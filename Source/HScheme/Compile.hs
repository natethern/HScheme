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

module Org.Org.Semantic.HScheme.Compile where
	{
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.SymbolExpression;
	import Org.Org.Semantic.HScheme.FunctorLambda;
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

	type Macro cm r m = [Object r m] -> cm (SchemeExpression r m (m (Object r m)));
	type Syntax cm r m = [Object r m] -> cm (Object r m);

{--
	data Definable r m = ValueDefinable (SchemeExpression r m (m (Object r m))) |
		SyntaxDefinable (Syntax cm r m);
--}
	newtype TopLevelAction cm r m = MkTopLevelAction {unTopLevelAction :: forall a.
	 ([Object r m] -> cm (SchemeExpression r m (m a))) ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m a))};

	type TopLevelMacro cm r m = [Object r m] -> TopLevelAction cm r m;

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
		body <- beginM bodyObj;
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
		expr <- compile obj;
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
		body <- beginM bodyObj;
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
		body <- beginM bodyObj;
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
		body <- beginM bodyObj;
		return (fSubstMapRecursive substMap binds body);
		};

	defineT ::
		(
		Build cm r,
		Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> TopLevelAction cm r m;
	defineT (sym,(val,())) = MkTopLevelAction (\beg objs -> do
		{
		valExpr <- compile val;
		body <- beg objs;
		return (liftF2 (\mvalue mlocma -> do
			{
			value <- mvalue;
			loc <- new value;
			mlocma (return loc);
			}) valExpr (fAbstract sym body));
		});

	begin ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 cm (SchemeExpression r m (m a)) -> 
	 (m (Object r m) -> (m a)) ->
	 (m (Object r m) -> (m a) -> (m a)) ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m a));
	begin none one _ [] = none;
	begin none one conn (obj:objs) = do
		{
		mpair <- getMaybeConvert obj;
		case mpair of
			{
			Just (sym,args) -> case getBinding ?toplevelbindings sym of
				{
				Just tlm -> unTopLevelAction (tlm args) (begin none one conn) objs;
				Nothing -> compileSequence objs;
				};
			Nothing -> compileSequence objs;
			};
		} where
		{
		compileSequence [] = do
			{
			exp <- compile obj;
			return (fmap one exp);
			};
		compileSequence objs = do
			{
			exp <- compile obj;
			exps <- begin none one conn objs;
			return (liftF2 conn exp exps);
			};
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
	 cm (SchemeExpression r m (m (Object r m)));
	beginM = begin (fail "bad begin") id (>>);

	beginList ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (SchemeExpression r m (m [Object r m]));
	beginList = begin
	 (return (return' (return [])))
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

	beginListM ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (SchemeExpression r m (m (Object r m)));
	beginListM obj = fmap' (fmap (\mlist -> mlist >>= getConvert)) (beginList obj);

	beginListEat ::
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
	beginListEat eat = begin
	 (return (return' (return ())))
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
top-level: define, load, [define-syntax]
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

	-- as fProductList
	execList :: (Monad m) =>
	 [m a] -> m [a];
	execList [] = return [];
	execList (ma:mas) = do
		{
		a <- ma;
		as <- execList mas;
		return (a:as);
		};

	doApply :: (Scheme m r
--	,?bindings :: Bindings r m
	) =>
	 m (Object r m) ->
	 m [Object r m] ->
	 m (Object r m);
	doApply mf margs = do
		{
		f <- mf;
		case f of
			{
			ProcedureObject proc -> do
				{
				args <- margs;
				proc args;
--				func <- runSymbolExpressionM mGetBindings proc;
--				func args;
				};
			_ -> fail "bad-apply-form";
			};
		};

	makeApply :: (Scheme m r) =>
	 SchemeExpression r m (m (Object r m)) ->
	 [SchemeExpression r m (m (Object r m))] ->
	 SchemeExpression r m (m (Object r m));
	makeApply f args = fApply (fmap doApply f) (fmap execList (fExtract args));

	compileApply ::
		(
		Build cm r,
		Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 Object r m -> [Object r m] -> cm (SchemeExpression r m (m (Object r m)));
	compileApply f arglist = do
		{
		fe <- compile f;
		ae <- sinkList compile arglist;
		return (makeApply fe ae);
		};

	compile ::
		(
		Build cm r,
		Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 Object r m -> cm (SchemeExpression r m (m (Object r m)));
	compile (SymbolObject sym) = return (fmap (\mloc -> do
		{
		loc <- mloc;
		get loc;
		}) (fSymbol sym));
	compile (PairObject head tail) = do
		{
		h <- get head;
		t <- get tail;
		marglist <- getMaybeConvert t;
		case marglist of
			{
			Nothing -> fail "not an argument list";
			Just arglist -> case h of
				{
				SymbolObject sym -> case getBinding ?syntacticbindings sym of
					{
					Just syntax -> do
						{
						obj <- syntax arglist;
						compile obj;
						};
					Nothing -> case getBinding ?macrobindings sym of
						{
						Just macro -> macro arglist;
						Nothing -> compileApply h arglist;
						};
					};
				_ -> compileApply h arglist;
				};
			};
		};
	compile a = case a of
		{
		BooleanObject _ -> return (return' (return a));
		NumberObject _ -> return (return' (return a));
		CharObject _ -> return (return' (return a));
		StringObject _ -> return (return' (return a));
		ByteArrayObject _ -> return (return' (return a));
		_ -> fail "can't evaluate form" -- throwArgError "cant-evaluate-form" [a];
		};
	}
