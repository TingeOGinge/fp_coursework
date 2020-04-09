--
-- MATHFUN
-- UP887062
--

import Text.Printf

--
-- Types (define Place type here)
--
type Coords = (Float, Float)
data Place = Place String Coords [Float] deriving (Eq,Ord,Show,Read)

data Tree = Null |
     Node (Place, Float) Tree Tree
     deriving (Eq,Show)

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

getCityRainAvg :: Place -> Float
getCityRainAvg (Place n c rain) = sum rain / fromIntegral (length rain)

displayCityRainAvg :: String -> [Place] -> String
displayCityRainAvg n p = printf "%.2f" (getCityRainAvg (getPlace n p))

format2dp :: Float -> String
format2dp x = printf "%8.2f" x

showRainfallList :: Place -> String
showRainfallList (Place n c r) = printf "%-10s %s" n (unwords (map format2dp r))

placesToString :: [Place] -> String
placesToString = unlines . map (showRainfallList)

dryInXDays :: Place -> Int -> Bool
dryInXDays (Place n c rain) x = rain!!(x-1) == 0

dryPlacesInXDays :: Int -> [Place] -> [Place]
dryPlacesInXDays x = filter (\p -> (dryInXDays p x))

updatePlaceRain :: Float -> Place -> Place
updatePlaceRain x (Place n c rain) = Place n c (init (x:rain))

updateAllPlaces :: [Float] -> [Place] -> [Place]
updateAllPlaces = zipWith (updatePlaceRain)

removePlace:: String -> [Place] -> [Place]
removePlace s = filter(\(Place n c r) -> n /= s)

addPlace :: Place -> [Place] -> [Place]
addPlace place list = list ++ [place]

getDistance :: Coords -> Coords -> Float
getDistance (x1, y1) (x2, y2) = sqrt( (x2-x1)^2 + (y2-y1)^2 )

assignDistance :: Coords -> Place -> (Place, Float)
assignDistance c1 (Place n c2 r) = (Place n c2 r, getDistance c1 c2)

insert :: (Place, Float) -> Tree -> Tree
insert p Null = Node p Null Null
insert (x,y) (Node (i,j) st1 st2)
  | y < j && st1 /= Null = (Node (i,j) (insert (x,y) st1) st2)
  | y < j && st1 == Null = (Node (i,j) (Node (x,y) Null Null) st2)
  | y > j && st2 /= Null = (Node (i,j) st1 (insert (x,y) st2))
  | y > j && st2 == Null = (Node (i,j) st1 (Node (x,y) Null Null))
  | otherwise = (Node (x,y) st1 st2)

inOrder :: Tree -> [Place]
inOrder (Node (x,y) st1 st2)
  | st1 /= Null && st2 /= Null = inOrder st1 ++ [x] ++ inOrder st2
  | st1 /= Null = inOrder st1 ++ [x]
  | st2 /= Null = [x] ++ inOrder st2
  | otherwise = [x]

listToSearchTree :: [(Place, Float)] -> Tree
listToSearchTree = foldr (insert) Null

binaryTreeSort :: [(Place, Float)] -> [Place]
binaryTreeSort = inOrder . listToSearchTree

findClosest :: Coords -> [Place] -> Place
findClosest c1 = head . binaryTreeSort . map(assignDistance c1)

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
  let dryPlaces = dryPlacesInXDays 2 testData
  let result = displayCityNames dryPlaces
  putStrLn result

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
  let closest = findClosest (50.9, (-1.2)) (dryPlacesInXDays 1 testData)
  putStrLn (placesToString [closest])


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
