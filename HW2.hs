--Charles Voege
--14098471

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

p1 = Just (Prgm [Proc "foo" ["n"] [stmt]] [Assign "x" (N 0), Call "foo" [(N 5)], Call "foo" [(N 5)]]) where
	stmt = Assign "x" (Add (I "x") (I "n"))

p2 = Just (Prgm [Proc "fact" ["n"] [stmt, stmt2, stmt3]] [Call "fact" [(N 10)]]) where
	stmt = Assign "x" (N 1)
	stmt2 = Assign "i" (N 1)
	stmt3 = While (Not b) [stmt4]
	b = Eq (I "i") (Add (I "n") (N 1))
	stmt4 = Assign "x" (Mul (I "x")(I "i"))

p3 = Just (Prgm [] [stmt]) where
	stmt = IfElse b [stmt2] []
	b = Eq (I "z") (I "z")
	stmt2 = Assign "x" (N 3)

p4 = Just(Prgm [Proc "fun" ["n"] [stmt]] [Call "fun" [(N 0)]]) where
	stmt = Call "fun" [(Add (I "n") (N 1))]

--In class on Friday, Dr. Harrison said it was Nothing.
p5 = Nothing

p6 = Just (Prgm [] [stmt]) where
	stmt = IfElse b [stmt2] [stmt3]
	b = And (Eq (I "a") (I "a")) (Or (Lt (I "b") (N 9)) (Gt (I "c") (N 0)))
	stmt2 = While (GtEq (I "n") (Sub (N 0) (N 6))) [stmt4]
	stmt4 = Assign "x" (Sub (I "x") (I "n"))
	stmt3 = Assign "x" (N 9)

--Doesn't follow the correct grammar
p7 = Nothing

p8 = Just (Prgm [] [stmt, stmt2]) where
	stmt = Assign "x" (N 1)
	stmt2 = Call "f3" [(N 1), (N 2), (N 3)]

p9 = Just (Prgm [proc, proc2, proc3, proc4] []) where
	proc = Proc "f0" [] [Assign "x" (N 0)]
	proc2 = Proc "f1" ["x"] [Assign "x" (I "x")]
	proc3 = Proc "f2" ["x", "y"] [Assign "x" (I "y")]
	proc4 = Proc "f3" ["x", "y", "z"] [Assign "x" (Add (Div (I "x") (I "y")) (I "z"))]
