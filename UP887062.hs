--
-- MATHFUN
-- UP887062
--

import Text.Printf
import Data.List

--
-- Types (define Place type here)
--
type Coords = (Float, Float)
data Place = Place String Coords [Int] deriving (Eq,Ord,Show,Read)

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

getPlace :: String -> [Place] -> Place
getPlace n = head . filter(\(Place name c r) -> n == name)

getAvg :: Place -> Float
getAvg (Place n c rain) = realToFrac (sum rain) / genericLength rain

placeAvgToString :: String -> [Place] -> String
placeAvgToString n p = printf "+ %s avg: %.2f" n (getAvg (getPlace n p))

formatCol :: Int -> String
formatCol x = printf "%5d" x

showRainfallList :: Place -> String
showRainfallList (Place n c r) = printf "%-12s %s" n (unwords (map formatCol r))

placesToString :: [Place] -> String
placesToString = unlines . map (showRainfallList)

dryInXDays :: Place -> Int -> Bool
dryInXDays (Place n c rain) x = rain!!(x-1) == 0

dryPlacesInXDays :: Int -> [Place] -> [Place]
dryPlacesInXDays x = filter (\p -> (dryInXDays p x))

updatePlaceRain :: Int -> Place -> Place
updatePlaceRain x (Place n c rain) = Place n c (init (x:rain))

updateAllPlaces :: [Int] -> [Place] -> [Place]
updateAllPlaces = zipWith (updatePlaceRain)

removePlace:: String -> [Place] -> [Place]
removePlace s = filter(\(Place n c r) -> n /= s)

addPlace :: Place -> [Place] -> [Place]
addPlace place list = list ++ [place]

getDistance :: Coords -> Coords -> Float
getDistance (x1, y1) (x2, y2) = sqrt( (x2-x1)^2 + (y2-y1)^2 )

assignDist :: Coords -> Place -> (Place, Float)
assignDist c1 (Place n c2 r) = (Place n c2 r, getDistance c1 c2)

compareDist :: (Place, Float) -> (Place, Float) -> Ordering
compareDist (_,a) (_,b) = compare a b

stripDist :: (Place, Float) -> Place
stripDist (x,_) = x

getClosest :: Coords -> [Place] -> Place
getClosest c1 = stripDist . head . sortBy(compareDist) . map(assignDist c1)

convertYCoord :: Float -> Int
convertYCoord n = round (50 - ((n - 48.2) * (50 / 12)))

convertXCoord :: Float -> Int
convertXCoord n = round ((n + 7.4) * (80 / 8))

--
--  Demo
--
--
demo :: Int -> IO ()
-- display the names of all the places
demo 1 = putStr (displayCityNames testData)

-- display, to two decimal places, the average rainfall in Cardiff
demo 2 = putStrLn (placeAvgToString "Cardiff" testData)

-- display all place names and their 7-day rainfall figures as a single string
demo 3 = putStrLn (placesToString testData)

-- display the names of all places that were dry two days ago
demo 4 = putStr(displayCityNames (dryPlacesInXDays 2 testData))

-- update the data with most recent rainfall (and remove oldest figures)
demo 5 = do
  let updatedPlaces = updateAllPlaces [0,8,0,0,5,0,0,3,4,2,0,8,0,0] testData
  putStrLn (placesToString updatedPlaces)

-- replace "Plymouth" with "Portsmouth" which has
-- location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
demo 6 = do
  let filteredPlaces = removePlace "Plymouth" testData
  let portsmouth = Place "Portsmouth" (50.8,(-1.1)) [0,0,3,2,5,2,1]
  let addPortsmouth = addPlace portsmouth filteredPlaces
  putStrLn (placesToString addPortsmouth)

-- display the name of the place closest to 50.9 (N), -1.3 (E) that was
-- dry yesterday
demo 7 = do
  let closestPlace = getClosest (50.9, (-1.2)) (dryPlacesInXDays 1 testData)
  putStrLn (placesToString [closestPlace])


demo 8 = do
  clearScreen
  displayMap testData


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

displayMap :: [Place] -> IO()
displayMap [] = goTo(0, 50)
displayMap ((Place n (y, x) r):xs) = do
  let output = placeAvgToString n [(Place n (x, y) r)]
  writeAt (convertXCoord x, convertYCoord y) output
  displayMap xs



--
-- Your user interface (and loading/saving) code goes here
--
