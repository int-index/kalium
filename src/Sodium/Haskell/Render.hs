module Sodium.Haskell.Render (render) where

import qualified Data.Char as C
import qualified Text.PrettyPrint as P
import Sodium.Haskell.Program

vsep = foldr (P.$+$) P.empty

render :: Program -> String
render (Program defs imports exts)
	= P.render
	$ vsep
	$ [renderExts exts]
	++ map renderImport imports
	++ map (\(Def def) -> renderDef def) defs

renderExts cs
	= P.hsep
	[ P.text "{-#"
	, P.text "LANGUAGE"
	, P.hsep
		$ P.punctuate P.comma
		$ map P.text cs
	, P.text "#-}"
	]

renderImport cs
	= P.text "import"
	P.<+> P.text cs

renderDef (ValueDef pat expr)
	= P.hsep
	[ renderPattern pat
	, P.text "="
	, renderExpression expr
	]

renderDef (GuardDef pat leafs)
	= renderPattern pat
	P.$+$
		( P.nest 4
		$ vsep
		$ map renderLeaf
		$ leafs
		)
	where
		renderLeaf (expr1, expr2)
			= P.hsep
			[ P.text "|"
			, renderExpression expr1
			, P.text "="
			, renderExpression expr2
			]

data Fixity
	= LFix
	| RFix
	| NFix
	deriving Eq

data Level
	= ALevel Integer Fixity
	| HLevel
	| SLevel

hlevel = (, HLevel)

renderExpr :: Expression -> (P.Doc, Level)

renderExpr (Access name)
	= hlevel (renderName name)

renderExpr (Tuple [expr])
	= renderExpr expr

renderExpr (Tuple exprs)
	= hlevel
	$ P.parens
	$ P.hsep
	$ P.punctuate P.comma
	$ map renderExpression
	$ exprs

renderExpr (Beta (Beta (Access "enumFromTo") exprFrom) exprTo)
	= let
		(operand1, level1) = renderExpr exprFrom
		(operand2, level2) = renderExpr exprTo
		wrap = \case
			SLevel -> P.parens
			_ -> id
		doc = wrap level1 operand1 P.<> P.text ".." P.<> wrap level2 operand2
	in hlevel (P.brackets doc)

renderExpr (Primary lit)
	= hlevel
	$ case lit of
		Quote cs -> P.text (show cs)
		INumber intSection -> P.text intSection
		FNumber intSection fracSection -> P.hcat
			[ P.text intSection
			, P.text "."
			, P.text fracSection
			]
		ENumber intSection fracSection eSign eSection -> P.hcat
			[ P.text intSection
			, P.text "."
			, P.text fracSection
			, P.text "e"
			, if eSign
				then P.text "+"
				else P.text "-"
			, P.text eSection
			]

renderExpr (Beta (Beta (Access op) expr1) expr2)
	| isInfix op = renderBinary op expr1 expr2
renderExpr (Beta expr1 expr2) = renderBinary "" expr1 expr2
renderExpr expr = (renderExpression expr, SLevel)

isLetter c = C.isAlpha c || c == '_'

isInfix op
	 = op `elem` ["elem", "div", "mod"]
	|| null op
	|| (not . isLetter) (head op)

renderOp op lhs rhs = lhs P.<+> rOp P.<+> rhs
	where rOp
		| null op = P.empty
		| isLetter (head op) = P.char '`' P.<> P.text op P.<> P.char '`'
		| otherwise = P.text op

renderBinary op expr1 expr2
	= (renderOp op lhs rhs, uncurry ALevel opfix)
	where
		opfix = whatfix op
		(operand1, level1) = renderExpr expr1
		(operand2, level2) = renderExpr expr2
		defaultHandler n m _
			| m > n = id
			| otherwise = P.parens
		advHandler n f m g
			| m > n = id
			| f == g && m == n = id
			| otherwise = P.parens
		(handler1, handler2) = case opfix of
			(n, NFix) -> (\a -> (a, a)) (defaultHandler n)
			(n, LFix) -> (advHandler n LFix, defaultHandler n)
			(n, RFix) -> (defaultHandler n, advHandler n RFix)
		wrap handler = \case
			SLevel -> P.parens
			HLevel -> id
			ALevel n fix -> handler n fix
		lhs = wrap handler1 level1 operand1
		rhs = wrap handler2 level2 operand2

whatfix op
	= maybe (9, LFix) id (lookup op fixtable)
	where fixtable =
		[ ("", (10, LFix))
		, ("+", (6, LFix))
		, ("-", (6, LFix))
		, ("*", (7, LFix))
		, ("/", (7, LFix))
		]


renderExpression (Lambda pats expr)
	= P.hsep
	[ P.hcat
		[ P.text "\\"
		, P.hsep $ map renderPattern pats
		]
	, P.text "->"
	, renderExpression expr
	]

renderExpression (Typed expr t)
	= P.hsep
	[ case renderExpr expr of
		(r, HLevel) -> r
		(r, _) -> P.parens r
	, P.text "::"
	, renderType t
	]

renderExpression (DoExpression [DoExecute expr])
	= renderExpression expr

renderExpression (DoExpression statements)
	= P.text "do"
	P.$+$ (P.nest 4 $ vsep $ map renderStatement statements)

renderExpression (PureLet valueDefs expr)
	= P.text "let"
	P.$+$ (P.nest 4 $ vsep $ map renderDef valueDefs)
	P.$+$ (P.text "in" P.<+> renderExpression expr)

renderExpression (IfExpression expr bodyThen bodyElse)
	= (P.text "if" P.<+> renderExpression expr)
	P.$+$ (P.text "then" P.<+> renderExpression bodyThen)
	P.$+$ (P.text "else" P.<+> renderExpression bodyElse)


renderExpression expr = fst (renderExpr expr)

renderName name
	| null name = P.empty
	| (not . isLetter) (head name) = P.parens (P.text name)
	| otherwise = P.text name

renderType = \case
	HsType cs  -> P.text cs
	HsUnit -> P.text "()"
	HsIO t -> renderName "IO" P.<+> renderType t

renderStatement (DoBind pat expr)
	= P.hsep
	[ renderPattern pat
	, P.text "<-"
	, renderExpression expr
	]

renderStatement (DoLet pat expr)
	= P.hsep
	[ P.text "let"
	, renderPattern pat
	, P.text "="
	, renderExpression expr
	]

renderStatement (DoExecute expr)
	= renderExpression expr

renderPattern (PatFunc name names)
	= renderName name P.<+> P.hsep (map renderName names)

renderPattern (PatTuple [name])
	= renderName name

renderPattern (PatTuple names)
	= P.parens
	$ P.hsep
	$ P.punctuate P.comma
	$ map renderName
	$ names
