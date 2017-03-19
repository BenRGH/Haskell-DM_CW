-- MATHFUN
-- UP780065

-- Imports
import Data.List
import Data.Char
import Control.Monad

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
filmsOfAFan fan filmsDB = filmsAsString (filter (\(Film _ _ _ fans) -> fan `elem` fans) filmsDB)

-- v - All the fans of a specified film
fansOfAFilm :: [Film] -> [Film] -> String
-- This takes a film and returns the list of fans
fansOfAFilm ((Film _ _ _ []):_) _ = "" -- Stops the recursive loop
fansOfAFilm ((Film t d r (fan:rest)):_) films = fan ++ "\n" ++ fansOfAFilm [(Film t d r rest)] films

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
-- Output: List of Fan
fansOfADirector _ [] = []
fansOfADirector name ((Film _ director _ fans):db)
  | director == name     = nub (fans ++ (fansOfADirector name db))
  -- Nub once again stops duplicates here
  | otherwise            = fansOfADirector name db

-- viii - List all directors with the number of their films a user is a fan of
directorsFanCnt :: String -> [Director] -> [Film] -> String
{- This function takes the name of a fan and the film database and returns
a list of all directors with the number of films a specified user is a fan of
in a pair -}
directorsFanCnt _ [] _ = ""
directorsFanCnt user (dir:rest) filmsDB = "\n" ++ dir ++ ": " ++ (show (length (filter (\(Film _ _ _ fans) -> user `elem` fans) (filter (\(Film _ dirs _ _) -> dir == dirs) filmsDB)))) ++ (directorsFanCnt user rest filmsDB)
-- ADD WHERE!

-- Demos

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
demo 8  = putStrLn (directorsFanCnt "Liz" (allDirectors testDatabase) testDatabase)


-- User Interface

main :: IO ()
main = do
-- Load and print Database
  filmsDBRaw <- readFile "DBFile.txt"
  let filmsDB = read filmsDBRaw :: [Film]
  putStrLn (filmsAsString filmsDB)

-- User name
  putStrLn "Please input your User Name:\n"
  name <- getLine
  if name == ""
      then do
          putStrLn "Incorrect User Name."
          main
  else do
      menu (nameValidation name) filmsDB


menu :: String -> [Film]-> IO ()
-- The main options menu for the UI
menu name filmsDB = do
    putStr "What would you like to do?\n\
    \1.Add film(s)\n\
    \2.View database\n\
    \3.Find films released after a year\n\
    \4.List my favourite films\n\
    \5.List the fans of a film\n\
    \6.Add a favourite film\n\
    \7.List fans of a director\n\
    \8.List all directors and the number of their films I'm a fan of\n\
    \9.Demos\n\
    \10.Exit\n\
    \Enter the corresponding number: "

    choice <- getLine
    case choice of
        "1" -> do
            addFilmIO name filmsDB
        "2" -> do
            putStrLn $ filmsAsString filmsDB
            menu name filmsDB
        "3" -> do
            putStrLn "Please enter a year: "
            year <- getLine
            if containsLetter year || not (isLength year 4)
                then do
                    putStrLn "That's not a year, try again."
                    menu name filmsDB
                else do
                    let yearInt = read year :: Int
                    putStrLn (releasedAfter yearInt filmsDB)
        "4" -> do
            putStrLn (filmsOfAFan name filmsDB)
        "5" -> do
            return expression
        "6" -> do
            return expression
        "7" -> do
            return expression
        "8" -> do
            return expression
        "9" -> do
            return expression
        "10" -> do
            writeFile "DBFile.txt" (show filmsDB)
        _ -> do
             putStrLn "Please enter a valid choice"
             menu name filmsDB


-- Exit - save to txt



addFilmIO :: String -> [Film] -> IO ()
addFilmIO name filmsDB = do
    putStrLn "-  Adding a film  -"
    putStrLn "Title: "
    title <- getLine
    if (title == "") || (title `elem` (allTitles filmsDB))
        then do
            putStrLn "Please enter a valid title not already in the database."
            addFilmIO name filmsDB
    else do
        putStrLn "Director: "
        director <- getLine
        if (director == "") || (containsNumber director)
            then do
                putStrLn "Please enter a valid name."
                addFilmIO name filmsDB
        else do
            putStrLn "Release date: "
            release <- getLine
            if release == "" ||  containsLetter release || not (isLength release 4)
                then do
                    putStrLn "Please enter a valid year."
                    addFilmIO name filmsDB
            else do
                fans <- (addFans True [])
                menu name (addFilm (Film title director (read release :: Int) fans) filmsDB)




-- "Helper" Functions


-- A better formatted list of all the films
getFilms :: [Film] -> IO ()
getFilms [] = putStrLn "You need to enter a valid Database"
getFilms films = putStrLn (filmsAsString films)

-- Search for one or more films by title
searchByTitle :: String -> [Film] -> [Film]
searchByTitle chosenTitle films = filter (\(Film title _ _ _) -> title == chosenTitle) films

-- Format fans of a director
fansOfDirAsStr:: [Fan] -> String
-- Used by vii, formats the list of fans to string with newlines
fansOfDirAsStr [] = [] -- Recursive stop
fansOfDirAsStr (x:xs) = x ++ "\n" ++ fansOfDirAsStr xs

-- Gets a list of all directors
allDirectors :: [Film] -> [Director]
-- Used by viii
allDirectors filmsDB = nub [dir | (Film _ dir _ _) <- filmsDB]

allTitles :: [Film] -> [Title]
allTitles filmsDB = [title | (Film title _ _ _) <- filmsDB]

-- Makes the username start with a capital letter (input validation)
nameValidation :: String -> String
nameValidation [] = []
nameValidation (first:rest) = toUpper first : map toLower rest

-- Checks if the input contains a number
containsNumber :: String -> Bool
containsNumber = any isNumber

-- Checks if input contains a letter
containsLetter :: String -> Bool
containsLetter = any isLetter

-- Checks if string is longer than set characters long
isLength :: String -> Int -> Bool
isLength (x:xs) limit =
    if (length xs) > (limit - 1)
        then do
            False
        else do
            True

-- Create a fans list from user input
addFans :: Bool -> [Fan] -> IO([Fan])
addFans more fans
    | more = do
        putStrLn "Add a fan? (y/n):"
        moreInput <- getLine
        if moreInput == "y"
            then do
                putStrLn "Enter fan's name: "
                name <- getLine
                if name == ""
                    then do
                        putStrLn "Please enter a valid name"
                        addFans True fans
                    else do
                        addFans True (fans ++ [nameValidation name])
            else do
                putStrLn "Recieving that as a no."
                addFans False fans

    | otherwise = return fans
