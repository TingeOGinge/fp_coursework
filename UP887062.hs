--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- UP887062
--

import Text.Printf

--
-- Types (define Place type here)
--
type Coords = (Float, Float)
data Place = Place String Coords [Float] deriving (Eq,Ord,Show,Read)

testData :: [Place]
testData = [
  Place "London"     (51.5, (-0.1)) [0, 0, 5, 8, 8, 0, 0 ],
  Place "Cardiff"    (51.5, (-3.2)) [12, 8, 15, 0, 0, 0, 2],
  Place "Norwich"    (52.6, (1.3))  [0, 6, 5, 0, 0, 0, 3],
  Place "Birmingham" (52.5, (-1.9)) [0, 2, 10, 7, 8, 2, 2],
  Place "Liverpool"  (53.4, (-3.0)) [8, 16, 20, 3, 4, 9, 2],
  Place "Hull"       (53.8, (-0.3)) [0, 6, 5, 0, 0, 0, 4],
  Place "Newcastle"  (55.0, (-1.6)) [0, 0, 8, 3, 6, 7, 5],
  Place "Belfast"    (54.6, (-5.9)) [10, 18, 14, 0, 6, 5, 2],
  Place "Glasgow"    (55.9, (-4.3)) [7, 5, 3, 0, 6, 5, 0],
  Place "Plymouth"   (50.4, (-4.1)) [4, 9, 0, 0, 0, 6, 5],
  Place "Aberdeen"   (57.1, (-2.1)) [0, 0, 6, 5, 8, 2, 0],
  Place "Stornoway"  (58.2, (-6.4)) [15, 6, 15, 0, 0, 4, 2],
  Place "Lerwick"    (60.2, (-1.1)) [8, 10, 5, 5, 0, 0, 3],
  Place "St Helier"  (49.2, (-2.1)) [0, 0, 0, 0, 6, 10, 0]
  ]

--
--  Your functional code goes here
--

displayCityNames :: [Place] -> String
displayCityNames = unlines . map(\(Place name c r) -> name)

getCityByName :: String -> [Place] -> Place
getCityByName n ((Place name c r):xs)
 | n == name = (Place name c r)
 | otherwise = getCityByName n xs

getCityRainAvg :: Place -> Float
getCityRainAvg (Place n c rainf) = sum rainf / fromIntegral (length rainf)

displayCityRainAvg :: String -> [Place] -> String
displayCityRainAvg n c = printf "%.2f" (getCityRainAvg (getCityByName n c))

format2dp :: Float -> String
format2dp x = printf "%8.2f" x

showRainfallList :: Place -> String
showRainfallList (Place n c r) = printf "%-10s %s" n (unwords (map format2dp r))

placesToString :: [Place] -> String
placesToString = unlines . map (showRainfallList)

dryInXDays :: Place -> Int -> Bool
dryInXDays (Place n c rain) x = rain!!(x-1) == 0

dryPlacesInXDays :: Int -> [Place] -> String
dryPlacesInXDays x = displayCityNames . filter (\p -> (dryInXDays p x))

updatePlaceRain :: Float -> Place -> Place
updatePlaceRain x (Place n c rain) = Place n c (init (x:rain))

updateAllPlaces :: [Float] -> [Place] -> [Place]
updateAllPlaces = zipWith (updatePlaceRain)

removePlace:: String -> [Place] -> [Place]
removePlace s = filter(\(Place n c r) -> n /= s)

addPlace :: Place -> [Place] -> [Place]
addPlace place list = place:list

updatePlace :: Place -> Place -> Place
updatePlace (Place n1 c1 r1) (Place n2 c2 r2) = Place n2 c2 r2

replacePlace :: String -> Place -> [Place] -> [Place]
replacePlace og new list
  = [if og == n then (updatePlace(getPlace og list) new) else (Place n c r) | (Place n c r) <- list]

--
--  Demo
--
--
demo :: Int -> IO ()
-- display the names of all the places
demo 1 = do
  let cities = displayCityNames testData
  putStrLn (cities)

-- display, to two decimal places, the average rainfall in Cardiff
demo 2 = do
  let result = displayCityRainAvg "Cardiff" testData
  putStrLn result

-- display all place names and their 7-day rainfall figures as a single string
demo 3 = putStrLn (placesToString testData)

-- display the names of all places that were dry two days ago
demo 4 = do
  let dryCities = dryPlacesInXDays 2 testData
  putStrLn dryCities

-- update the data with most recent rainfall (and remove oldest rainfall figures)
demo 5 = do
  let updatedPlaces = updateAllPlaces [0,8,0,0,5,0,0,3,4,2,0,8,0,0] testData
  putStrLn (placesToString updatedPlaces)

demo 6 = do
  let alteredPlaces = replacePlace "Plymouth" (Place "Portsmouth" (50.8,(-1.1)) [0,0,3,2,5,2,1]) testData
  putStrLn (placesToString alteredPlaces)
-- demo 7 = -- display the name of the place closest to 50.9 (N), -1.3 (E)
--          -- that was dry yesterday
-- demo 8 = -- display the rainfall map


--
-- Screen Utilities (use these to do the rainfall map - note that these do
-- not work in WinGHCi on Windows, so use GHCi.)
--

--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text


--
-- Your rainfall map code goes here
--



--
-- Your user interface (and loading/saving) code goes here
--
