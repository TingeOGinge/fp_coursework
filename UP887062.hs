--
-- MATHFUN
-- UP887062
-- All demos are complete and functioning
-- A fully functioning UI has also been implemented and can be run with the
-- main program
--

-- Used to format the String outputs
import Text.Printf

-- Used as part of the UI to validate inputs (confirmed with Matthew Poole
-- that this is okay)
import Text.Read

-- Imported for general list manipulation
import Data.List

--
-- Types (define Place type here)
--
type Name = String
type Coords = (Float, Float)
type Rainfall = [Int]
data Place = Place Name Coords Rainfall deriving (Show,Read)

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

-- Used to display the names of all currently recorded places (demo 1)
placeNames :: [Place] -> String
placeNames = unlines . map(\(Place name _ _) -> name)

--------------------------------------------------------------------------------

-- Retrieve a place by its name
-- UI prevents duplicate names and the hardcoded data can be verified
-- Consequently no duplicates are expected in this function
getPlace :: Name -> [Place] -> Place
getPlace n = head . filter(\(Place name _ _) -> n == name)

-- Retrieve the average rainfall of a given place (demo 2)
getAvg :: Place -> Float
getAvg (Place _ _ r) = realToFrac (sum r) / genericLength r

-- Finds a place by its name and returns the average rainfall as a String
placeAvgToStr :: Name -> [Place] -> String
placeAvgToStr n places = printf "+ %s avg: %.2f" n (getAvg (getPlace n places))

--------------------------------------------------------------------------------

-- The following four functions are used to format the data, showing each place
-- with the name and rainfall figures (demo 3)
formatCol :: Int -> String
formatCol x = printf "%5d" x

formatRain :: Rainfall -> String
formatRain = unwords . map formatCol

showRainfallList :: Place -> String
showRainfallList (Place n _ r) = printf "%-12s %s" n (formatRain r)

-- Takes the entire lists and maps them to the formatted strings before unlining
placesToString :: [Place] -> String
placesToString = unlines . map (showRainfallList)

--------------------------------------------------------------------------------

-- The two functions below are used to retrieve locations that were dry a given
-- amount of days ago (demo 4)
dryInXDays :: Place -> Int -> Bool
dryInXDays (Place _ _ rain) x = rain!!(x-1) == 0

dryPlacesInXDays :: Int -> [Place] -> [Place]
dryPlacesInXDays x = filter (\p -> (dryInXDays p x))

--------------------------------------------------------------------------------

-- The following two functions are used to update the latest rainfall figures
-- of all places in the given list (demo 5)
updatePlaceRain :: Int -> Place -> Place
updatePlaceRain x (Place n c rain) = Place n c (init (x:rain))

updateAllPlaces :: [Int] -> [Place] -> [Place]
updateAllPlaces = zipWith (updatePlaceRain)

--------------------------------------------------------------------------------

-- The following two functions complete demo 6.
-- This solution was favoured over a single line solution using list
-- comprehension, as I decided it would be better to access each step
-- individually if needed, rather than save two lines of code.

-- This function is used to remove a place from a list when given it's name
removePlace:: Name -> [Place] -> [Place]
removePlace s = filter(\(Place n _ _) -> n /= s)

-- This function takes a place and adds it to the list.
-- UI handles the input validation so a valid Place is expected
addPlace :: Place -> [Place] -> [Place]
addPlace place list = list ++ [place]

--------------------------------------------------------------------------------

-- The following 5 functions are used to locate the nearest place to given Coords
-- First we calculate and assign the distance of each Place to the target Coords
-- Then we sort the list by these distances
-- Finally we take the first (closest) element and return the place with the
-- distance value stripped
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


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- The following functional code is used to support the user interface and
-- rainfall map. I opted to keep it here to ensure complete separation of
-- functional and IO code

-- To convert the coordinates I used the following basic formula
-- 1: Adjust all values 'n' so the lowest value becomes 1
-- 2: Calculate the difference between the highest and lowest values and
-- divided the screen size by this value to get the 'conversion factor'
-- 3: Multiply the new 'n' value by the 'conversion factor'
-- 3b (Y Coords only): For the Y coordinates I then flipped the values by the
-- screen height
-- 4: use the round function to return an Int
convertYCoord :: Float -> Int
convertYCoord n = round (50 - ((n - 48.2) * (50 / 12)))

convertXCoord :: Float -> Int
convertXCoord n = round ((n + 7.4) * (80 / 8))

-- The two functions are used to validate the UI input of place names
isPlace :: Name -> Place -> Bool
isPlace s (Place n _ _) = s == n

placeExists :: Name -> [Place] -> Bool
placeExists s = foldr (||) False . map(isPlace s)

-- The next two functions are used to validate numerical input for coordinates
-- or rainfall figures from the UI
validInt :: String -> Bool
validInt s = Nothing /= (readMaybe s :: Maybe Int)

validFloat :: String -> Bool
validFloat s = Nothing /= (readMaybe s :: Maybe Float)

--
--  Demo
--
--
demo :: Int -> IO ()
-- display the names of all the places
demo 1 = putStr (placeNames testData)

-- display, to two decimal places, the average rainfall in Cardiff
demo 2 = putStrLn (placeAvgToStr "Cardiff" testData)

-- display all place names and their 7-day rainfall figures as a single string
demo 3 = putStr (placesToString testData)

-- display the names of all places that were dry two days ago
demo 4 = putStr(placeNames (dryPlacesInXDays 2 testData))

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

-- Display a rainfall map showing all places relative to their locations
demo 8 = do
  clearScreen
  displayMap testData


--
-- Screen Utilities (use these to do the rainfall map - note that
-- these do not work in WinGHCi on Windows, so use GHCi.)
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

-- Iterate through the list displaying each place using the converted Coords
displayMap :: [Place] -> IO()
displayMap [] = goTo(0, 50)
displayMap ((Place n (y, x) r):xs) = do
  let output = placeAvgToStr n [(Place n (y, x) r)]
  writeAt (convertXCoord x, convertYCoord y) output
  displayMap xs

--
-- Your user interface (and loading/saving) code goes here
--

getRainfallUpdates :: [Place] -> [Int] -> IO [Int]
getRainfallUpdates [] rainfallFigs = return rainfallFigs
getRainfallUpdates ((Place name c r):xs) rainfallFigs = do
  putStr (printf "Enter the rainfall figure for %s: " name)
  i <- getLine
  if (validInt i && not("-" `isInfixOf` i)) then do
    let newList = rainfallFigs ++ [(read i :: Int)]
    getRainfallUpdates xs newList
    else do
      putStrLn "Invalid input"
      getRainfallUpdates ((Place name c r):xs) rainfallFigs

getCoords :: IO Coords
getCoords = do
  putStr "Enter (N): "
  n <- getLine
  putStr "Enter (E): "
  e <- getLine
  if (validFloat n && validFloat e) then
    return ((read n :: Float), (read e :: Float))
    else do
      putStrLn "Invalid input"
      getCoords

getRainfall :: Int -> [Int] -> IO [Int]
getRainfall 0 rainfall = return rainfall
getRainfall n rainfall = do
  putStr "Input rainfall data: "
  r <- getLine
  if (validInt r && not("-" `isInfixOf` r)) then do
    let newRainfall = rainfall ++ [(read r :: Int)]
    getRainfall (n-1) newRainfall
    else do
      putStrLn "Invalid input"
      getRainfall n rainfall

getRainfallDay :: IO Int
getRainfallDay = do
  putStrLn "How many days ago?"
  x <- getLine
  if (validInt x && x `elem` ["1","2","3","4","5","6","7"]) then
    return (read x :: Int)
    else do
      putStrLn "Invalid input"
      getRainfallDay

createPlace :: [Place] -> IO Place
createPlace list = do
  putStr "Enter the name of the new place: "
  name <- getLine
  if (placeExists name list) then do
    putStrLn (printf "%s already exists" name)
    createPlace list
    else do
      coords <- getCoords
      rainfall <- getRainfall 7 []
      return (Place name coords rainfall)

option :: String -> [Place] -> IO [Place]
option "1" list = do
  putStr (placeNames list)
  return list

option "2" list = do
  putStr "Enter the name of your place: "
  place <- getLine
  if (placeExists place list) then do
    putStrLn (placeAvgToStr place list)
    return list
    else do
      putStrLn (printf "%s was not found, please try again" place)
      option "2" list

option "3" list = do
  putStr (placesToString list)
  return list

option "4" list = do
  days <- getRainfallDay
  putStr(placeNames (dryPlacesInXDays days list))
  return list

option "5" list = do
  rainfallFigs <- getRainfallUpdates list []
  let newList = updateAllPlaces rainfallFigs list
  putStrLn (placesToString newList)
  return newList

option "6" list = do
  putStr "Enter the name of the city you would like to remove: "
  place <- getLine
  if (placeExists place list) then do
    let filteredList = removePlace place list
    newPlace <- createPlace list
    let newList = addPlace newPlace filteredList
    putStrLn (placesToString newList)
    return newList
    else do
      putStrLn "City not found"
      option "6" list

option "7" list = do
  coords <- getCoords
  day <- getRainfallDay
  let dryList = dryPlacesInXDays day list
  let closestPlace = getClosest coords dryList
  putStrLn (placesToString [closestPlace])
  return list

option "8" list = do
  clearScreen
  displayMap list
  writeAt (0,50) "Press enter to continue"
  wait <- getLine
  return list

option _ list = do
  putStrLn "Invalid input"
  return list

menuData :: [String]
menuData = [
  "\nEnter a number to complete that action",
  "0 = Exit the program",
  "1 = List the names of all places available",
  "2 = Return the average rainfall of a given place",
  "3 = Return all places and their 7-day rainfall figures",
  "4 = Return a list of names for all places that were dry a given amount of days ago",
  "5 = Update rainfall figures for all places",
  "6 = Replace existing place with a new place",
  "7 = Given a location find the closest place that was dry yesterday",
  "8 = Display a rainfall map"
  ]

menu :: [Place] -> IO [Place]
menu list = do
  putStrLn (unlines menuData)
  choice <- getLine
  if choice == "0" then
    return list
    else do
      clearScreen
      newList <- option choice list
      menu newList

main :: IO ()
main = do
  raw <- readFile "places.txt"
  let list = (read raw :: [Place])
  clearScreen
  putStr (placeNames list)
  newList <- menu list
  writeFile "places.txt" (show newList)
