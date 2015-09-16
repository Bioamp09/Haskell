module HW0 where
import Data.Char
data Date = Date Int Int Int deriving Show

halloween = Date 10 31 2015

--instance Show Date where
--	show(Date m d y) = show (Date m d y)

gooddate :: Date -> Maybe Date
gooddate (Date m d y) = if 1 <= m && m <= 12 && 1 <= d && d <= 31 && y >= 0
			then
				Just (Date m d y)
			else
				Nothing

--instance Show Month where <-- went over this line in class
--	blah blah balh

--1A) testdate function is exactly the same as gooddate...
testdate :: Date -> Maybe Date
testdate (Date m d y) = if 1 <= m && m <= 12 && 1 <=d && d<= 31 && y >= 0
			then
				Just (Date m d y)
			else
				Nothing

--betterdate function takes in the testdate function and returns Maybe Date but works exactly like testdate
--probably fix this later
--1B)
betterdate :: Date -> Maybe Date
betterdate (Date m d y) = if 1 <= m && m <= 12 && 1 <= d && d <= 31 && y >= 0
			then
				testdate(Date m d y)
			else
				Nothing

--bestdate function that behaves the same as gooddate but uses the as-pattern
--1C)
bestdate :: Date -> Maybe Date
bestdate date@(Date m d y) = if 1 <= m && m <= 12 && 1 <=d && d <= 31 && y >= 0
			then
				testdate(date)
			else
				Nothing
 
--declaration of Month type
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

--Instance of show to show the month given the input
--1D)
instance Show Month where
--	show [Jan .. Dec] = ["January", "Febuary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	show Jan 	= 	"January"
	show Feb 	= 	"Febuary"
	show Mar 	= 	"March"
	show Apr 	= 	"April"
	show May 	= 	"May"
	show Jun 	= 	"June"
	show Jul 	= 	"July"
	show Aug 	= 	"August"
	show Sep 	= 	"September"
	show Oct 	= 	"October"
	show Nov 	= 	"November"
	show Dec 	= 	"December"

data NewDate = NewDate Month Int Int deriving Show
christmas = NewDate Dec 25 2015

--1E)
--goodnewdate :: NewDate -> Maybe NewDate
--goodnewdate (NewDate((Month) d y))= if 1 <= (Month) && (Month) <= 12 && 1 <= d && d <= 31 && y >= 0
--			then
--				Just (NewDate((Month) d y))
--			else
--
--				Nothing

--2)
nextlet :: Char -> Char
nextlet x = if isDigit(x)
		then
			x
		else
			succ x

--3)
digitval :: Char -> Int
digitval x = if isDigit(x)
		then 
			digitToInt(x)
		else
			-1

--4)
twine :: (a -> b) -> (a -> c) -> a -> (b, c)
twine a b (a = (a(b), a(c))

--5)
cond :: 
