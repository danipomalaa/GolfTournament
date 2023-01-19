module Main where
import System.Random
import System.Directory
import Data.List
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
                        registrationPlayer "Registration Player"
                        main
            ("2") -> do 
                        -- putStrLn "Your menu is Pairing List "
                        pairingList "Pairing List"
                        main
            ("3") -> do 
                        -- putStrLn "Your menu is Score Sheet "
                        registrationPlayer "Score Sheet"
                        main
            ("4") -> do
                        -- putStrLn "Your menu is Leader Board "
                        registrationPlayer "Leader Board"
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
                    
data EntryList = EntryList {regNumber:: Int, name:: String, gender:: String, hc:: Int, flight:: String } deriving Show

---------------------------- Registration Menu -----------------------------

registrationPlayer :: String ->  IO()
registrationPlayer arg = do
                    putStrLn "=================================== "
                    putStrLn ("         Menu "++ arg)
                    putStrLn "=================================== "
                    putStrLn "Please choose menu in below"
                    putStrLn "1. Add Player"
                    putStrLn "2. View List Player"
                    putStrLn "3. Edit Player"
                    putStrLn "4. Delete Player"
                    putStrLn "5. Back To Main Menu"
                    putStrLn "=================================== "
                    pilih <- getLine
                    case (pilih) of 
                        ("1") -> do
                                    putStrLn "====Add Player==="
                                    dataAdd <- entryData 1 0
                                    putStrLn "--------Please Check your entry-----"
                                    putStrLn ("1. Reg Number \t: "++ show (regNumber dataAdd) ++"\n2. Name \t:"++ (name dataAdd)++ "\n3. Gender\t:"++ (gender dataAdd)++"\n4. HC\t\t:"++ show (hc dataAdd)++"\n5.Flight\t: "++(flight dataAdd))
                                    putStrLn "------------------------------------"
                                    putStrLn "if your data is correct, please press key `1`"
                                    putStr "Your choose is : "
                                    saveData <- getLine
                                    if (saveData == "1") then
                                        do  
                                            appendFile "player.txt" (show (regNumber dataAdd)++" "++(removeSpaceInput (name dataAdd))++" "++ (gender dataAdd)++" "++ show (hc dataAdd)++" "++(flight dataAdd)++ "\n")
                                            putStrLn "Save Data"
                                    else 
                                        putStrLn "Cancel"
                                    previewData
                                    registrationPlayer arg
                        ("2") -> do
                                    previewData
                                    registrationPlayer arg
                        ("3") -> do
                                    previewData
                                    putStr "Input Reg Number : "
                                    editRegNumber <- getLine

                                    dataPlayer <- readFile "player.txt"
                                    let dataPlayerList = convertTextToArray dataPlayer

                                    changeData 1 (read editRegNumber) dataPlayerList
                                    previewData
                                    -- writeFile "player.txt" ""

                        ("4") -> do
                                    previewData
                                    putStr "Input Reg Number : "
                                    editRegNumber <- getLine

                                    dataPlayer <- readFile "player.txt"
                                    let dataPlayerList = convertTextToArray dataPlayer
                                    changeData 2 0 dataPlayerList
                                    previewData
                        ("5") -> putStrLn "Back To Main Menu"
                        (_) -> registrationPlayer arg

entryData :: Int -> Int -> IO(EntryList)
entryData 1 _ = do
                putStr "Insert Your Name : "
                name <- getLine 
                putStr "Insert Your Gender (Men or Ladies) : "
                gender <- getLine
                putStrLn "***********Flight Category************"
                putStrLn "Fligth A from HC 1 to 10"
                putStrLn "Fligth B from HC 11 to 19"
                putStrLn "Fligth C from HC 20 to 28"
                putStrLn "**************************************"
                putStr "Insert Your HC (1 - 28) :  "
                hc <- getLine
                let flight = if (read hc) >=1 && (read hc) <10 then "A"
                                else if (read hc) >=11 && (read hc) <20 then "B"
                                else "C"
                regNumber <- rollDice
                let datareturn = (EntryList { regNumber= regNumber, name = name, gender = gender, hc= (read (hc)), flight= flight})
                return datareturn
entryData 2 arg = do
                putStr "Insert Your Name : "
                name <- getLine 
                putStr "Insert Your Gender (Men or Ladies) : "
                gender <- getLine
                putStrLn "***********Flight Category************"
                putStrLn "Fligth A from HC 1 to 10"
                putStrLn "Fligth B from HC 11 to 19"
                putStrLn "Fligth C from HC 20 to 28"
                putStrLn "**************************************"
                putStr "Insert Your HC (1 - 28) :  "
                hc <- getLine
                let flight = if (read hc) >=1 && (read hc) <10 then "A"
                                else if (read hc) >=11 && (read hc) <20 then "B"
                                else "C"
                let datareturn = (EntryList { regNumber= arg, name = name, gender = gender, hc= (read (hc)), flight= flight})
                return datareturn
entryData _ _ = return(EntryList{})

changeData :: Int -> Int -> [EntryList] -> IO()
changeData 1 argRegNumber arg = do
                                    let dataList = filter (\x-> not ((regNumber x)  == argRegNumber)) arg
                                    dataEdit <- entryData 2 argRegNumber
                                    let _data = (dataList ++ [dataEdit])
                                    let dataString = (_data >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")))
                                    -- saveData _data
                                    writeFile "player.txt" dataString
                                    print ("Edit Data RegNumber "++ (show argRegNumber) ++ " success")
changeData 2 argRegNumber arg = do
                                    let dataList = filter (\x-> not ((regNumber x) == argRegNumber)) arg
                                    -- saveData dataList
                                    -- let dataString = _data >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")) 
                                    let dataString = (dataList >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")))
                                    writeFile "player.txt" dataString
                                    print ("Delete Data RegNumber "++ (show argRegNumber) ++ " success")
changeData _ _ [] = return()
                                
saveData :: [EntryList] -> IO(String)
saveData [] = return("")
saveData arg = return (arg >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")))                      

profileData :: Int -> IO()
profileData 0 = return()
profileData arg = do
                dataPlayer <- readFile "player.txt"
                let dataPlayerList = convertTextToArray dataPlayer
                let profile = filter (\x-> (regNumber x) == arg) dataPlayerList
                if (length profile == 0) then
                    print "Data is not found. "
                else
                    do
                        let dataObject = profile!!0
                        putStrLn ("1. Reg Number \t:"++ show ((regNumber dataObject)) ++ "\n"++
                                 "2. Name \t:"++ (name dataObject)++ "\n"++
                                 "3. Gender \t:"++ (gender dataObject)++ "\n"++
                                 "4. HC \t\t:"++ show ((hc dataObject))++ "\n"++
                                 "5. Flight \t:"++ (flight dataObject))

previewData :: IO()
previewData = do
                dataPlayer <- readFile "player.txt"
                putStrLn "===========List Player================"
                -- print (dataPlayer)
                putStrLn "------------------------------------------------------"
                putStrLn "|No|Reg|Name\t\t\t|Gender\t|HC\t|Flight |"
                putStrLn "------------------------------------------------------"
                printEntryList 0 $ convertTextToArray dataPlayer
                putStrLn "======================================================"
                putStrLn ("\t\tTotal Player : "++ show (length $ convertTextToArray dataPlayer)++ " Person")
                putStrLn "======================================================"


sortEntryList :: [[String]] -> [EntryList]
sortEntryList [] = []
sortEntryList (arg:xs) = (EntryList { regNumber=(read (arg!!0)), name = arg!!1, gender = arg!!2, hc= (read (arg!!3)), flight= arg!!4}) : sortEntryList xs

printEntryList :: Int -> [EntryList] -> IO()
printEntryList _ [] = return()
printEntryList index (x:xs) =  do 
                                    let indexNo = if ((index+1)<10) then "0"++(show (index+1)) else show (index+1)
                                    let namePlayer = if (length (name x) < 9) then (name x) ++ "\t\t\t" else (name x) ++ "\t\t"
                                    putStrLn ("|"++indexNo++ "|"++ show (regNumber x) ++"|"++ namePlayer ++ "|"++ (gender x) ++ "\t|"++ (show (hc x))++"\t|"++ (flight x)++"\t|")
                                    printEntryList (index+1) xs

extractFlightList :: [EntryList] -> Int -> Int -> [String]
extractFlightList arg val1 val2 = map (\z-> show (name z)) $ filter (\x-> (hc x) > val1 && (hc x)<= val2) arg

getFlightList :: [EntryList] -> Int -> Int -> [EntryList]
getFlightList arg val1 val2 = filter (\x-> (hc x) > val1 && (hc x)<= val2) arg

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
                                    orderPlayer <- orderPlayerList' dataPlayer
                                    drawData' 0 countMaxGroup ( orderPlayer >>= (\x-> x ++ []) )  

                                    viewPairingList
                                    pairingList arg
                        ("2") -> do
                                    viewPairingList
                                    pairingList arg
                        ("3") -> putStrLn "Back To Main Menu"
                        (_) -> pairingList arg

viewPairingList :: IO()
viewPairingList = do
                    putStrLn "====List Pairing ==="
                    dataPairing <- listDirectory "./group"
                    printPairingList 0 dataPairing

randomIntList :: Int -> IO [Int]
randomIntList n = do
                     gen <- newStdGen
                     return (take n (randomRs (100, 999) gen))

createTupple :: [Int] -> [String] -> [(Int,String)]
createTupple [] [] = []
createTupple (x:xs) (y:ys) = [(x,y)] ++ (createTupple xs ys) 

createTupple' :: [Int] -> [EntryList] -> [(Int,Int)]
createTupple' [] [] = []
createTupple' (x:xs) (y:ys) = [(x, (regNumber y))] ++ (createTupple' xs ys) 

removeDirectoryWithFiles :: [String] -> IO()
removeDirectoryWithFiles [] = return()
removeDirectoryWithFiles (x:xs) = do
                                    removeFile ("./group/"++x)
                                    removeDirectoryWithFiles xs

printPairingList :: Int -> [String] -> IO()
printPairingList _ [] = return ()
printPairingList index (x:xs) = do
                        dataPlayerString <- readFile "player.txt"
                        let dataPlayer = convertTextToArray dataPlayerString
                        dataPairingString <- readFile ("group/"++ x)
                        let dataPairing = words dataPairingString
                        putStrLn ("Group "++ (show (index+1)))
                        putStrLn ("---------------------------------------------------------")
                        putStrLn "|No|Reg|Name\t\t\t|Gender\t|HC\t|Flight |"
                        putStrLn ("---------------------------------------------------------")
                        let dataGroup = dataPairing >>= (\y-> filter (\x-> (regNumber x) == (read y)) dataPlayer)
                        -- print dataGroup
                        -- putStrLn (tablePairing 0 dataPairing)
                        tablePairing' 0 dataGroup
                        putStrLn ("---------------------------------------------------------")
                        -- putStrLn ((words dataPairing) >>= (\arg-> arg++"\n"))
                        printPairingList (index+1) xs

tablePairing :: Int -> [String] -> String
tablePairing _ [] = ""
tablePairing index (x:xs) = "|0"++show (index+1)++"|"++x++"\t\t\t|\n" ++ tablePairing (index+1) xs

tablePairing' :: Int -> [EntryList] -> IO()
tablePairing' _ [] = return()
tablePairing' index (x:xs) = do 
                                let namePlayer = if (length (name x) < 9) then (name x) ++ "\t\t\t" else (name x) ++ "\t\t"
                                putStrLn ("|0"++show (index+1)++"|"++ show (regNumber x) ++"|"++ namePlayer ++ "|"++ (gender x) ++ "\t|"++ (show (hc x))++"\t|"++ (flight x)++"\t|")
                                tablePairing' (index+1) xs
    -- "|0"++show (index+1)++"|"++ (name x)++"\t\t\t|"++ (flight x) ++"\t|"++ (show (hc x))++"\t|\n" ++ tablePairing' (index+1) xs

                                
orderPlayerList :: String -> IO [[String]]
orderPlayerList " " = return []
orderPlayerList dataPlayer = 
    do

        let flightA = (extractFlightList (convertTextToArray dataPlayer) 1 10)
        randomIntA <- randomIntList $ length flightA
        let orderFligthA = (sort $ createTupple randomIntA flightA) >>= (\(a,b)-> [b])

        let flightB = (extractFlightList (convertTextToArray dataPlayer) 11 20)
        randomIntB <- randomIntList $ length flightB
        let orderFligthB = (sort $ createTupple randomIntB flightB) >>= (\(a,b)-> [b])

        let flightC = (extractFlightList (convertTextToArray dataPlayer) 21 30)
        randomIntC <- randomIntList $ length flightC
        let orderFligthC = (sort $ createTupple randomIntC flightC) >>= (\(a,b)-> [b])

        putStrLn "Flight A : "
        print flightA
        print orderFligthA
        
        putStrLn "Flight B : "
        print flightB
        print orderFligthB
        putStrLn "Flight C : "
        print flightC
        print orderFligthC

        return ([orderFligthA]++[orderFligthB]++[orderFligthC])

orderPlayerList' :: String -> IO [[Int]]
orderPlayerList' " " = return []
orderPlayerList' dataPlayer = 
    do

        let flightA = (getFlightList (convertTextToArray dataPlayer) 1 10)
        
        randomIntA <- randomIntList $ length flightA
        -- print flightA
        -- print randomIntA
        let orderFligthA = (sort $ createTupple' randomIntA flightA) >>= (\(a,b)-> [b])
        let dataTypeOrderFlightA = orderFligthA >>= (\y-> filter (\x-> (regNumber x) == y) flightA) 

        let flightB = (getFlightList (convertTextToArray dataPlayer) 10 20)
        randomIntB <- randomIntList $ length flightB

        -- print flightB
        -- print randomIntB
        let orderFligthB = (sort $ createTupple' randomIntB flightB) >>= (\(a,b)-> [b])
        let dataTypeOrderFlightB = orderFligthB >>= (\y-> filter (\x-> (regNumber x) == y) flightB) 

        let flightC = (getFlightList (convertTextToArray dataPlayer) 20 28)
        randomIntC <- randomIntList $ length flightC

        -- print flightC
        -- print randomIntC
        let orderFligthC = (sort $ createTupple' randomIntC flightC) >>= (\(a,b)-> [b])
        let dataTypeOrderFlightC = orderFligthC >>= (\y-> filter (\x-> (regNumber x) == y) flightC) 

        return ([orderFligthA]++[orderFligthB]++[orderFligthC])


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

                                if( (index+1) < group) then drawData (index+1) group xs
                                else drawData 0 group xs

drawData' :: Int -> Int -> [Int] -> IO()
drawData' _ 0 _= putStrLn "Data Tidak Ada"
drawData' _ _ []= putStrLn "Selesai"
drawData' index group (x:xs)= 
                            do 
                                appendFile ("group/group"++(show (index+1))++".txt")  ((show x)++" ")
                                if( (index+1) < group) then drawData' (index+1) group xs
                                else drawData' 0 group xs
-- orderFligthA >>= (\y-> filter (\x-> (regNumber x) == y) flightA) 

---------------------------- End Pairing Menu -----------------------------
