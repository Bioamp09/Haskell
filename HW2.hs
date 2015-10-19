module HW2 where

type Id = String

data Prgm = Prgm [Proc] [Stmt]
	deriving (Eq, Show)

data Proc = Proc Id [Id] [Stmt]
	deriving (Eq, Show)

data Exp = N Int | I String
	| Add Exp Exp | Mul Exp Exp | Sub Exp Exp | Div Exp Exp
	deriving (Eq, Show)

data BExp = T | F | And BExp BExp | Or BExp BExp | Not BExp | Lt Exp Exp | LtEq Exp Exp| Gt Exp Exp | GtEq Exp Exp | Eq Exp Exp
	deriving (Eq, Show)

data Stmt = Assign Id Exp | Call Id [Exp] | IfElse BExp [Stmt] [Stmt] | While BExp [Stmt]
	deriving (Eq, Show)

p0, p1, p2, p3, p4, p5, p6, p7, p8, p9 :: Maybe Prgm

p0 = Just (Prgm [] [stmt1]) where
	stmt1 = IfElse b [stmt2] [stmt3]
	b = Or bool2 bool3
	bool2 = Eq int1 int2
	int1 = I "foo"
	int2 = N 2
	bool3 = Gt int3 int4
	int3 = I "foo"
	int4 = N 10
	stmt2 = Assign "bar" (Mul (N 2) (Mul (I "foo") (N 2)))
	stmt3 = Assign "bar" (N 4)

p1 = 

p2 = undefined

p3 = undefined

p4 = undefined

p5 = undefined

p6 = undefined

p7 = undefined

p8 = undefined

p9 = undefined
