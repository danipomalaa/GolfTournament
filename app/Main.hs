module Main where

import System.Random
import System.Directory

--------------------------- Main Menu ----------------------------------------

main :: IO ()
main = do 
        putStrLn "=================================== "
        putStrLn "Welcome Golf Tournament Application "
        putStrLn "=================================== "

        putStrLn "Please choose menu in below"
        putStrLn "1. Registration Player"
        putStrLn "2. Pairing List"
        putStrLn "3. Score Sheet"
        putStrLn "4. Leader Board"
        putStrLn "5. Exit"
        putStrLn "=================================== "
        pilih <- getLine
        case (pilih) of
            ("1") -> do
                        -- putStrLn "Your menu is Registration Player "
                        entryPlayer "Registration Player"
                        main
            ("2") -> do 
                        -- putStrLn "Your menu is Pairing List "
                        pairingList "Pairing List"
                        main
            ("3") -> do 
                        -- putStrLn "Your menu is Score Sheet "
                        entryPlayer "Score Sheet"
                        main
            ("4") -> do
                        -- putStrLn "Your menu is Leader Board "
                        entryPlayer "Leader Board"
                        main
            ("5") -> do
                        -- putStrLn "Your menu is Exit Application \n"
                        putStrLn "Thank you for using this application"
                        putStrLn "=============Bye bye ==============="

--------------------------- End Main Menu ----------------------------------------

--------------------------- Other Function ----------------------------------------

rollDice :: IO Int
rollDice = getStdRandom (randomR (100,999))

removeSpaceInput :: String -> String
removeSpaceInput [] = []
removeSpaceInput (x:xs) = if x ==' ' then removeSpaceInput xs
                    else [x] ++ removeSpaceInput xs

--------------------------- End Other Function ----------------------------------------
                    
data EntryList = EntryList {regNumber:: Int, name:: String, gender:: String, hc:: Int } deriving Show

---------------------------- Registration Menu -----------------------------

entryPlayer :: String ->  IO()
entryPlayer arg = do
                    putStrLn "=================================== "
                    putStrLn ("         Menu "++ arg)
                    putStrLn "=================================== "
                    putStrLn "Please choose menu in below"
                    putStrLn "1. Add Player"
                    putStrLn "2. View List Player"
                    putStrLn "3. Reset Player"
                    putStrLn "4. Back To Main Menu"
                    putStrLn "=================================== "
                    pilih <- getLine
                    case (pilih) of 
                        ("1") -> do
                                    putStrLn "====Add Player==="
                                    putStrLn "Insert Your Name : "
                                    name <- getLine 
                                    putStrLn "Insert Your Gender (Men or Ladies) : "
                                    gender <- getLine
                                    putStrLn "Insert Your HC : (1 - 28)  "
                                    putStrLn "***********Note************"
                                    putStrLn "Fligth A from HC 1 to 10"
                                    putStrLn "Fligth B from HC 11 to 19"
                                    putStrLn "Fligth C from HC 20 to 28"
                                    putStrLn "***************************"
                                    hc <- getLine
                                    regNumber <- rollDice
                                    putStrLn "--------Please Check your entry-----"
                                    putStrLn ("1. RegisterNumber: "++ show regNumber ++"\n2. Name:"++name++ "\n3. Gender:"++gender++"\n4. HC:"++hc)
                                    putStrLn "------------------------------------"
                                    putStrLn "if your data is correct, please press `1` or press `any key` for cancel process"
                                    saveData <- getLine
                                    if (saveData == "1") then
                                        do
                                            appendFile "player.txt" (show regNumber++" "++(removeSpaceInput name)++" "++gender++" "++hc++"\n")
                                            putStrLn "Save Data"
                                    else 
                                        putStrLn "Cancel"
                                    previewData
                                    entryPlayer arg
                        ("2") -> do
                                    previewData
                                    entryPlayer arg
                        ("3") -> do
                                    writeFile "player.txt" ""
                        ("4") -> putStrLn "Back To Main Menu"
                        (_) -> entryPlayer arg

previewData :: IO()
previewData = do
                dataPlayer <- readFile "player.txt"
                putStrLn "===========List Player================"
                -- print (dataPlayer)
                putStrLn "------------------------------------------------"
                putStrLn "|No|Reg|Name\t\t\t|Gender\t|HC\t|"
                putStrLn "------------------------------------------------"
                printEntryList 0 $ convertTextToArray dataPlayer
                putStrLn "================================================"
                putStrLn ("\t\tTotal Player : "++ show (length $ convertTextToArray dataPlayer)++ " Person")
                putStrLn "================================================"

sortEntryList :: [[String]] -> [EntryList]
sortEntryList [] = []
sortEntryList (arg:xs) = (EntryList { regNumber=(read (arg!!0)), name = arg!!1, gender = arg!!2, hc= (read (arg!!3))}) : sortEntryList xs

printEntryList :: Int -> [EntryList] -> IO()
printEntryList _ [] = return()
printEntryList index (x:xs) =  do 
                                    let indexNo = if ((index+1)<10) then "0"++(show (index+1)) else show (index+1)
                                    let namePlayer = if (length (name x) < 9) then (name x) ++ "\t\t\t" else (name x) ++ "\t\t"
                                    putStrLn ("|"++indexNo++ "|"++ show (regNumber x) ++"|"++ namePlayer ++ "|"++ (gender x) ++ "\t|"++ (show (hc x))++"\t|")
                                    printEntryList (index+1) xs

extractFlightList :: [EntryList] -> Int -> Int -> [String]
extractFlightList arg val1 val2 = map (\z-> show (name z)) $ filter (\x-> (hc x) > val1 && (hc x)<= val2) arg

convertTextToArray :: String -> [EntryList]
convertTextToArray "" = []
convertTextToArray arg = sortEntryList dataEntry where
                         dataEntry = fmap words (lines arg)

------------------------   End Registration Menu -----------------------------

---------------------------- Pairing Menu -----------------------------

pairingList :: String -> IO()
pairingList arg = do
                    putStrLn "=================================== "
                    putStrLn ("         Menu "++ arg)
                    putStrLn "=================================== "
                    putStrLn "Please choose menu in below"
                    putStrLn "1. Draw Pairing List"
                    putStrLn "2. View Pairing List"
                    putStrLn "3. Back To Main Menu"
                    putStrLn "=================================== "
                    pilih <- getLine
                    case (pilih) of 
                        ("0") -> do
                                    dataPlayer <- readFile "player.txt"
                                    putStrLn "====List Player==="
                                    putStrLn ("Total Player : "++ show (length $ convertTextToArray dataPlayer)++ " Person")

                                    print ("Flight A->", (orderPlayerList dataPlayer)!!0 )
                                    print ("Flight B->", (orderPlayerList dataPlayer)!!1)
                                    print ("Flight C->", (orderPlayerList dataPlayer)!!2)

                                    writeFile "entries/entriesA.txt" ""
                                    writeFile "entries/entriesB.txt" ""
                                    writeFile "entries/entriesC.txt" ""
                                    appendFile "entries/entriesA.txt" (unwords ((orderPlayerList dataPlayer)!!0))
                                    appendFile "entries/entriesB.txt" (unwords ((orderPlayerList dataPlayer)!!1))
                                    appendFile "entries/entriesC.txt" (unwords ((orderPlayerList dataPlayer)!!2))
                                    pairingList arg
                        ("1") -> do
                                    putStrLn "====Draw Pairing List==="
                                    dataPlayer <- readFile "player.txt"
                                    let countMaxGroup = (length $ convertTextToArray dataPlayer) `div` 4   
                                    putStrLn ("Total Group :"++ show (countMaxGroup))
                                    -- Clear Directory -----
                                    directoryData <- listDirectory "."
                                    if("group" `elem` directoryData) then
                                        do
                                            putStrLn "Folder group is Exist"
                                            filesGroup <- listDirectory "./group"
                                            removeDirectoryWithFiles filesGroup
                                    else 
                                        do
                                            putStrLn "Folder group is not Exist"
                                            createDirectory "group"
                                    -- Create Directory -----
                                    clearDrawData 0 countMaxGroup
                                    drawData 0 countMaxGroup ( orderPlayerList dataPlayer >>= (\x-> x ++ []) )  
                                    pairingList arg
                        ("2") -> do
                                    putStrLn "====List Pairing ==="
                                    dataPairing <- listDirectory "./group"
                                    -- print dataPairing
                                    printPairingList dataPairing
                                    pairingList arg
                        ("3") -> putStrLn "Back To Main Menu"
                        (_) -> pairingList arg

removeDirectoryWithFiles :: [String] -> IO()
removeDirectoryWithFiles [] = return()
removeDirectoryWithFiles (x:xs) = do
                                    removeFile ("./group/"++x)
                                    removeDirectoryWithFiles xs

printPairingList :: [String] -> IO()
printPairingList [] = do
                        putStrLn "-----------------------------------" 
                        return ()
printPairingList (x:xs) = do
                        dataPairing <- readFile ("group/"++ x)
                        putStrLn ("-------Pairing "++x ++ "----")
                        putStrLn ((words dataPairing) >>= (\arg-> arg++"\n"))
                        printPairingList xs

orderPlayerList :: String -> [[String]]
orderPlayerList " " = []
orderPlayerList dataPlayer = 
    do
        let flightA = (extractFlightList (convertTextToArray dataPlayer) 1 10)
        let flightB = (extractFlightList (convertTextToArray dataPlayer) 11 20)
        let flightC = (extractFlightList (convertTextToArray dataPlayer) 21 30)
        ([flightA]++[flightB]++[flightC])

clearDrawData :: Int -> Int -> IO()
clearDrawData _ 0 = putStrLn "Selesai"
clearDrawData index group = do 
                                writeFile ("group/group"++(show (index+1))++".txt") ""  
                                clearDrawData (index+1) (group-1)

data FlightList = FlightList {groupNumber:: Int, player:: [EntryList] } deriving Show

drawData :: Int -> Int -> [String] -> IO()
drawData _ 0 _= putStrLn "Data Tidak Ada"
drawData _ _ []= putStrLn "Selesai"
drawData index group (x:xs)= 
                            do 
                                appendFile ("group/group"++(show (index+1))++".txt")  (x++" ")
                                print ("Group-"++show (index+1)++ " "++x )

                                if( (index+1) < group) then drawData (index+1) group xs
                                else drawData 0 group xs

---------------------------- End Pairing Menu -----------------------------
