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
isPerm f = 

--2) a one liner using something to do with map
pId :: Color -> Color
pId = id
isId :: (Color -> Color) -> Bool
isId f = 

--3)
runAgain :: Int -> (a -> a) -> (a -> a)
runAgain n f = if n <= 0 then id else f . (runAgain (n - 1) f)

order :: (Color -> Color) -> Maybe Int
order f = 

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

--replace undefined with the right answer
heads :: Hydra -> Int
heads Head 				= 1
heads (Neck1 Color Hydra) 		= undefined
heads (Neck2 Color Hydra1 Hydra2) 	= Hydra1 + Hydra2

--5)
apocephalate :: Hydra -> Hydra
apocephalate Head 			= undefined --2 grow back
apocephalate (Neck1 Color Hydra) 	= undefined --
apocephalate (Neck2 Color Hydra1 Hydra2)= undefined --

--6)
data Snake = Segment Color Snake | Tail
	deriving(Eq, Show)

toSnake :: Hydra -> Maybe Snake
toSnake Hydra = 

--7)
toList :: Snake -> [Color]
toList Snake = 

--8)
uroborize :: Snake -> Snake
uroborize Snake = 
