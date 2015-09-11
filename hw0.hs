data Date = Date Int Int Int
halloween = Date 10 31 2015

gooddate :: Date -> Maybe Date
gooddate (Date m d y) = if 1 <= m && m <= 12 && 1 <= d && d <= 31 && y >= 0
			then
				Just (Date m d y)
			else
				Nothing

--testdate :: Date -> Bool
--testdate (Date m d y) = if gooddate (Date m d y) == Maybe (Date m d y)
--			then
--				True
--			else
--				False

--betterdate :: Date -> Bool
--betterdate (Date m d y) = if betterdate (Date m d y)
--			then
--				True
--			else
--				False

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Nov | Dec
