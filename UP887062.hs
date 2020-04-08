--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--



--
-- Types (define Place type here)
--

testData :: [Place]
testData = [ ... the 14 Place values ... ]

--
--  Your functional code goes here
--




--
--  Demo
--

demo :: Int -> IO ()
demo 1 = -- display the names of all the places
demo 2 = -- display, to two decimal places, the average rainfall in Cardiff
demo 3 = putStrLn (placesToString testData)
demo 4 = -- display the names of all places that were dry two days ago
demo 5 = -- update the data with most recent rainfall 
         --[0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)
demo 6 = -- replace "Plymouth" with "Portsmouth" which has 
         -- location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
demo 7 = -- display the name of the place closest to 50.9 (N), -1.3 (E) 
         -- that was dry yesterday
demo 8 = -- display the rainfall map


--
-- Screen Utilities (use these to do the rainfall map - note that these do 
-- not work in WinGHCi on Windows, so use GHCi.)
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
 
