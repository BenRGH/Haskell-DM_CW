-- MATHFUN
-- UP780065

-- Imports
import Data.List
--import Data.Set


-- Types
type Title = String
type Director = String
type Release = Int
type Fan = String


-- Defining the Algebraic type Film with constructor taking all the film properties
data Film = Film Title Director Release [Fan]
  deriving(Show, Read)


-- Film database
testDatabase :: [Film]
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Sam", "Olga", "Tim"],Film "The Fly" "David Cronenberg" 1986 ["Garry", "Dave", "Zoe", "Kevin", "Emma"],Film "Body Of Lies" "Ridley Scott" 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"],Film "Avatar" "James Cameron" 2009 ["Dave", "Amy", "Liz"],Film "Titanic" "James Cameron" 1997 ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"],Film "The Departed" "Martin Scorsese" 2006 ["Wally", "Liz", "Kevin", "Tim", "Emma"],Film "Aliens" "Ridley Scott" 1986 ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"],Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo", "Wally", "Emma"],Film "Prometheus" "Ridley Scott" 2012 ["Kevin", "Tim", "Emma", "Jo", "Liz"],Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave", "Amy", "Garry", "Ian", "Neal"],Film "Bridge of Spies" "Steven Spielberg" 2015 ["Wally", "Sam", "Dave", "Neal"],Film "Jaws" "Steven Spielberg" 1975 ["Dave", "Jo", "Zoe", "Wally", "Emma", "Kate"],Film "The Martian" "Ridley Scott" 2015 ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"],Film "The BFG" "Steven Spielberg" 2016 ["Sam", "Wally", "Dave", "Jo", "Kate"],Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe"],Film "Gladiator" "Ridley Scott" 2000 ["Olga", "Neal", "Kate", "Heidi", "Bill", "Sam", "Zoe"],Film "The Green Mile" "Frank Darabont" 1999 ["Kevin", "Tim", "Emma", "Heidi"] ,Film "True Lies" "James Cameron" 1994 ["Sam", "Dave"],Film "Super 8" "J J Abrams" 2011 ["Kevin", "Tim", "Emma", "Olga", "Heidi"],Film "Minority Report" "Steven Spielberg" 2002 ["Kevin", "Kate", "Tim", "Emma", "Olga", "Jenny", "Zoe"],Film "War Horse" "Steven Spielberg" 2011 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"],Film "Silence" "Martin Scorsese" 2016 ["Wally", "Emma", "Tim", "Heidi", "Bill", "Olga", "Jo"],Film "The Terminal" "Steven Spielberg" 2004 ["Kate", "Dave", "Jo", "Wally", "Emma"],Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz", "Jo"],Film "Hugo" "Martin Scorsese" 2011 ["Wally", "Sam"]]


-- CORE FUNCTIONAL CODE

-- i - Add film
addFilm :: Film -> [Film] -> [Film]
{-This takes the new film properties and the current "database" and
puts it all in a new "database" that it outputs-}
addFilm newFilm filmsDB = filmsDB ++ [newFilm]

{- To add a film use the example below:
addFilm ("scooby doo adventures" "garblar!" 1890 ["Bob", "Jack", "Kate"]) testDatabase
-}

-- ii - Give all films in DB
filmsAsString :: [Film] -> String
{- Takes the film database and converts it into a readable string with fans
as a number rather than a list-}
filmsAsString [] = "" --The DB is empty or there's an error
filmsAsString ((Film title director release fans) : db) = "\n" ++ title ++ "\nDirector: " ++ director ++ "\nRelease Date: " ++ show release ++ "\nNumber of fans: " ++ show (length fans) ++ "\n" ++ filmsAsString db

-- iii - give all films released *after* a specified year
releasedAfter :: Int -> [Film] -> String
-- Takes the films DB and only returns the films released after the given year
releasedAfter year filmsDB = filmsAsString [(Film t d release f) | (Film t d release f) <- filmsDB, release > year]
-- Uses list comprehension to return all films where the release is more than year

-- iv - All films a user is a fan of
filmsOfAFan :: Fan -> [Film] -> String
-- This shows us all the films that a specific person is a fan of
filmsOfAFan fan filmsDB = filmsAsString (filter (\(Film t d r fans) -> fan `elem` fans) filmsDB)

-- v - All the fans of a specified film
fansOfAFilm :: [Film] -> [Film] -> String
-- This takes a film and returns the list of fans
fansOfAFilm ((Film t d r []):db) _ = "" -- Stops the recursive loop
fansOfAFilm ((Film t d r (fan:rest)):db) films = fan ++ "\n" ++ fansOfAFilm [(Film t d r rest)] films

-- vi - Add fan to a film
addFan :: String -> String -> [Film] -> [Film]
-- This takes the name of a fan and adds it to the list in a films properties
-- Arguments: Fan name, name of the Film they're a fan of and the film database
addFan name chosenFilm [] = [] --Stops recursion
addFan name chosenFilm ((Film title d r fans):db)
  | title == chosenFilm   = ((Film title d r (nub(fans ++ [name]))):db)
  -- nub prevents duplicates
  | otherwise             = addFan name chosenFilm (db ++ [(Film title d r fans)])
 {- In the above line we are adding the film we just checked to the end of the list
 so that we keep the list of films the same-}

-- vii - Find a director's fans
fansOfADirector :: String -> [Film] -> [Fan]
-- Finds all the fans of a director from their films
-- Arguments: Director's name, film database, list of fans
fansOfADirector _ [] = []
fansOfADirector name ((Film t director r fans):db)
  | director == name     = nub (fans ++ (fansOfADirector name db))
  -- Nub once again stops duplicates here
  | otherwise            = fansOfADirector name db

-- viii - List all directors with the number of their films a user is a fan of
directorsFanCnt :: String -> [Film] -> [Film]
{- This function takes the name of a fan and the film database and returns
a list of all directors with the number of films a specified user is a fan of
in a pair -}
directorsFanCnt user filmsDB = filmsDB















demo :: Int -> IO ()
demo 1  = putStrLn (filmsAsString $ addFilm (Film "Alien: Covenant" "Ridley Scott" 2017 []) testDatabase)
-- Dollar sign is used as a kind of bracket-replacer
demo 2  = putStrLn (filmsAsString testDatabase)
demo 3  = putStrLn (releasedAfter 2008 testDatabase)
demo 4  = putStrLn (filmsOfAFan "Liz" testDatabase)
demo 5  = putStrLn (fansOfAFilm (searchByTitle "Jaws" testDatabase) testDatabase)
demo 6  = putStrLn (filmsAsString $ addFan "Liz" "The Fly" testDatabase)
demo 66 = putStrLn (filmsAsString $ addFan "Liz" "Avatar" testDatabase)
demo 7 = putStrLn (fansOfDirAsStr (fansOfADirector "James Cameron" testDatabase))
--demo 8  = putStrLn all directors & no. of their films that "Liz" is a fan of



-- User Interface

main :: IO ()
main = putStrLn ("Main locked and ready" ++ "\n")




-- "Helper" Functions

-- A better formatted list of all the films
getFilms :: [Film] -> IO ()
getFilms [] = putStrLn "You need to enter a valid Database"
getFilms films = putStrLn (filmsAsString films)

-- Search for one or more films by title
searchByTitle :: String -> [Film] -> [Film]
searchByTitle chosenTitle films = filter (\(Film title director release fans) -> title == chosenTitle) films

-- Format fans of a director
fansOfDirAsStr:: [Fan] -> String
-- Used by vii, formats the list of fans to string with newlines
fansOfDirAsStr [] = [] -- Recursive stop
fansOfDirAsStr (x:xs) = x ++ "\n" ++ fansOfDirAsStr xs
