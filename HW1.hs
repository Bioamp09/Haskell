--Charles Voege
--14098471
import Data.List

data Color = Blue | Green | Red | Ochre | Winedark
	deriving(Eq, Enum, Show)

p1 :: Color -> Color
p1 c = case c of
	Blue 		-> 	Green
	Green 		-> 	Red
	Red		->	Ochre
	Ochre		->	Winedark
	Winedark	->	Blue

p2 :: Color -> Color
p2 c = case c of
	Blue		->	Green
	Green		->	Blue
	Red		->	Red
	Ochre		->	Winedark
	Winedark	->	Ochre

notPerm :: Color -> Color
notPerm c = case c of
	Blue		->	Green
	Green		->	Blue
	Red		->	Red
	Ochre		->	Blue
	Winedark	->	Ochre

--1) Dr. Harrison went over this in class, yay for me!
--something about having a length 5
isPerm :: (Color -> Color) -> Bool
isPerm f = length (nub (map f [Blue ..])) == length [Blue ..]

--2) a one liner using something to do with map
pId :: Color -> Color
pId = id

isEqualColor :: (Color, Color) -> Bool
isEqualColor (x, y) = x == y

--yay for anonymous function calling and figuring out that $ acts like parenthesis!
isId :: (Color -> Color) -> Bool
isId f = foldl (\y z -> y && z) True $ map (\(x, y) -> x == y) $ zip [Blue ..] $ map f [Blue ..]

--3)
runAgain :: Int -> (a -> a) -> (a -> a)
runAgain n f = if n <= 0 then id else f . (runAgain (n - 1) f)

order :: (Color -> Color) -> Maybe Int
order f = if not $ isPerm f then Nothing else Just $ counterz f Blue 1
	where
		counterz :: (Color -> Color) -> Color -> Int -> Int
		counterz f c num = if (runAgain num f) c == c
				then num
				else counterz f c (num + 1)

--4)
data Hydra = Head | Neck1 Color Hydra | Neck2 Color Hydra Hydra
	deriving(Eq, Show)

spot :: Hydra
spot = Neck2 Blue
	(Neck2 Ochre Head Head)
	(Neck2 Red
		(Neck2 Green
			(Neck2 Red Head Head)
			(Neck2 Green
				(Neck1 Blue Head)
				(Neck2 Ochre Head Head)))
		(Neck1 Blue	(Neck2 Ochre Head Head)))

--replace undefined on right side of equals sign with the right answer
heads :: Hydra -> Int
heads Head 			= 1
heads (Neck1 _ hydra) 		= heads hydra
heads (Neck2 _ hydra1 hydra2) 	= (heads hydra1) + (heads hydra2)

--5)
apocephalate :: Hydra -> Hydra
apocephalate Head 			 	= Neck2 Blue Head Head
apocephalate (Neck1 color hydra) 		= Neck1 color (apocephalate hydra)
apocephalate (Neck2 color hydra1 hydra2)	= Neck2 color (apocephalate hydra1) (apocephalate hydra2)

--6)
data Snake = Segment Color Snake | Tail
	deriving(Eq, Show)

toSnake :: Hydra -> Maybe Snake
toSnake Head			= 	Just (Tail)
toSnake (Neck1 color hydra)	=	snakeAcc (Segment color Tail) hydra
--replace below case statement with helper function, will be easier according to Harrison
--		case toSnake hydra of
--		Just snake 	-> 	Just (Segment color snake)
--		Nothing		->	Nothing
toSnake (Neck2 _ _ _)		=	Nothing


--Helper function for toSnake
snakeAcc :: Snake -> Hydra -> Maybe Snake
snakeAcc s Head					= Just s
snakeAcc s (Neck1 c h) 				= Just s' 
	where	
		s' 				= Segment c s
		snakeAcc s h			= Just s
		--snakeAcc s (Neck2 _ _ _) 	= Nothing
snakeAcc s (Neck2 _ _ _)			= Nothing

--7)
toList :: Snake -> [Color]
toList (Segment color snake) 			= [color] ++ toList(snake)
toList Tail 					= []

--Hints Harrison said to do, uroborize snake = snake <some helper function> snake
--8)
uroborize :: Snake -> Snake
uroborize Tail		 		= Tail
--for some reason this errors out, if we have time, fix it, if not, oh well.
--uroborize (Segment c s) 		= Segment c (uroborize s) 
uroborize snake				= snake +++ uroborize snake

--Helper function Harrison said to fix up during office hours Friday
(+++) :: Snake -> Snake -> Snake
Tail +++ s = s
(Segment c s1) +++ s2 = Segment c (s1 +++ s2)
