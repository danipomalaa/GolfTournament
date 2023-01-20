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
        -- putStrLn "3. Score Sheet"
        -- putStrLn "4. Leader Board"
        putStrLn "3. Exit"
        putStrLn "=================================== "
        putStr "Your choose is : "
        pilih <- getLine
        case (pilih) of
            ("1") -> do
                        registrationPlayer "Registration"
                        main
            ("2") -> do 
                        pairingList "Pairing List"
                        main
            -- ("3") -> do 
            --             registrationPlayer "Score Sheet"
            --             main
            -- ("4") -> do
            --             registrationPlayer "Leader Board"
            --             main
            ("3") -> do
                        -- putStrLn "Your menu is Exit Application \n"
                        putStrLn "Thank you for using this application"
                        putStrLn "=============Bye bye ==============="
            (_) -> main

--------------------------- End Main Menu ----------------------------------------

--------------------------- Other Function ----------------------------------------

rollDice :: IO Int
rollDice = getStdRandom (randomR (1000,9999))

removeSpaceInput :: String -> String
removeSpaceInput [] = []
removeSpaceInput (x:xs) = if x ==' ' then removeSpaceInput xs
                    else [x] ++ removeSpaceInput xs

removeTextExtentionFile :: String -> String
removeTextExtentionFile [] = []
removeTextExtentionFile (x:xs) = if x =='.' || x =='t' || x =='x' || x =='t' then removeTextExtentionFile xs
                    else [x] ++ removeTextExtentionFile xs

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
                    putStr "Your choose is : "
                    pilih <- getLine
                    case (pilih) of 
                        ("1") -> do
                                    putStrLn "====Add Player==="
                                    -- dataAdd <- entryData 1 0

                                    putStrLn "Insert Your Name : "
                                    nameAdd <- getLine 
                                    putStrLn "Insert Your Gender (Men or Ladies) : "
                                    genderAdd <- getLine
                                    putStrLn "***********Flight Category************"
                                    putStrLn "Fligth A from HC 1 to 10"
                                    putStrLn "Fligth B from HC 11 to 19"
                                    putStrLn "Fligth C from HC 20 to 28"
                                    putStrLn "**************************************"
                                    putStrLn "Insert Your HC (1 - 28) :  "
                                    hcAdd <- getLine
                                    let flightAdd = if (read hcAdd) >=1 && (read hcAdd) <11 then "A"
                                                    else if (read hcAdd) >=11 && (read hcAdd) <20 then "B"
                                                    else "C"
                                    regNumberAdd <- rollDice 
                                    let dataAdd = (EntryList { regNumber= regNumberAdd, name = nameAdd, gender = genderAdd, hc= (read (hcAdd)), flight= flightAdd})

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
                                    putStrLn "Press any key to continue...."
                                    getLine
                                    registrationPlayer arg
                        ("2") -> do
                                    previewData
                                    putStrLn "Press any key to continue...."
                                    getLine
                                    registrationPlayer arg
                        ("3") -> do
                                    previewData
                                    putStrLn "Input Reg Number : "
                                    editRegNumber <- getLine
                                    profileData (read editRegNumber)
                                    dataPlayer <- readFile "player.txt"
                                    let dataPlayerList = convertTextToArray dataPlayer

                                    dataChange <- (changeData 1 (read editRegNumber) dataPlayerList)
                                    print dataChange
                                    writeFile "player.txt" dataChange
                                    -- previewData
                                    putStrLn "Press any key to continue...."
                                    getLine
                                    registrationPlayer arg

                        ("4") -> do
                                    previewData
                                    putStrLn "Input Reg Number : "
                                    editRegNumber <- getLine

                                    profileData (read editRegNumber)
                                    putStrLn "are you sure delete this data? Press key `1` to Yes or any key to cancel "
                                    deleteAction <- getLine
                                    case (deleteAction) of 
                                        ("1") -> do
                                                    dataPlayer <- readFile "player.txt"
                                                    let dataPlayerList = convertTextToArray dataPlayer
                                                    dataChange <- (changeData 2 (read editRegNumber) dataPlayerList)
                                                    print dataChange
                                                    writeFile "player.txt" dataChange
                                                    -- previewData
                                                    putStrLn "Press any key to continue...."
                                                    getLine
                                                    registrationPlayer arg
                                        (_) -> registrationPlayer arg
                        ("5") -> putStrLn "Back To Main Menu"
                        ("6")-> do 
                                    writeFile "player.txt" ""
                                    registrationPlayer arg
                        (_) -> registrationPlayer arg

changeData :: Int -> Int -> [EntryList] -> IO(String)
changeData 1 argRegNumber arg = do
                                    let dataList = filter (\x-> not ((regNumber x)  == argRegNumber)) arg
                                    let profile = (filter (\x-> (regNumber x) == argRegNumber) arg)!!0
                                    putStrLn "=================================== "
                                    putStrLn ("Press key `1` to change name")
                                    putStrLn ("Press key `2` to change gender")
                                    putStrLn ("Press key `3` to change HC")
                                    putStrLn ("Press key `4` to change all data")
                                    putStrLn ("Press any key to cancel.....")
                                    putStrLn "=================================== "
                                    changeChoose <- getLine
                                    case (changeChoose) of
                                        ("1") -> do
                                                    putStrLn ("Change Name "++ (name profile)++ " to :")
                                                    changeName <- getLine
                                                    let dataEdit = EntryList {regNumber = (regNumber profile), name = changeName, gender = (gender profile), hc = (hc profile), flight = (flight profile)}
                                                    let _data = (dataList ++ [dataEdit])
                                                    let dataString = (_data >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")))
                                                    return dataString
                                        ("2") -> do  
                                                    putStrLn ("Change Gender "++ (gender profile)++ " to :")
                                                    changeGender <- getLine 
                                                    let dataEdit = EntryList {regNumber = (regNumber profile), name = (name profile), gender = (changeGender), hc = (hc profile), flight = (flight profile)}
                                                    let _data = (dataList ++ [dataEdit])
                                                    let dataString = (_data >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")))
                                                    return dataString
                                        ("3") -> do  
                                                    putStrLn "***********Flight Category************"
                                                    putStrLn "Fligth A from HC 1 to 10"
                                                    putStrLn "Fligth B from HC 11 to 19"
                                                    putStrLn "Fligth C from HC 20 to 28"
                                                    putStrLn "**************************************"
                                                    putStrLn ("Change HC "++ show (hc profile) ++ " to :")
                                                    changeHC <- getLine 
                                                    let changeFlight = if (read changeHC) >=1 && (read changeHC) <11 then "A"
                                                                else if (read changeHC) >=11 && (read changeHC) <20 then "B"
                                                                else "C"
                                                    let dataEdit = EntryList {regNumber = (regNumber profile), name = (name profile), gender = (gender profile), hc = (read changeHC), flight = changeFlight}
                                                    let _data = (dataList ++ [dataEdit])
                                                    let dataString = (_data >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")))
                                                    return dataString
                                        ("4") -> do
                                                    putStrLn ("Change Name "++ (name profile)++ " to :")
                                                    changeName <- getLine
                                                    putStrLn ("Change Gender "++ (gender profile)++ " to :")
                                                    changeGender <- getLine 
                                                    putStrLn "***********Flight Category************"
                                                    putStrLn "Fligth A from HC 1 to 10"
                                                    putStrLn "Fligth B from HC 11 to 19"
                                                    putStrLn "Fligth C from HC 20 to 28"
                                                    putStrLn "**************************************"
                                                    putStrLn ("Change HC "++ show (hc profile) ++ " to :")
                                                    changeHC <- getLine 
                                                    let changeFlight = if (read changeHC) >=1 && (read changeHC) <10 then "A"
                                                                    else if (read changeHC) >=11 && (read changeHC) <20 then "B"
                                                                    else "C"
                                                    let dataEdit = (EntryList { regNumber= (regNumber profile), name = (changeName), gender = changeGender, hc= (read (changeHC)), flight= changeFlight})
                                                    let _data = (dataList ++ [dataEdit])
                                                    let dataString = (_data >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")))
                                                    return dataString
                                        (_) -> do
                                                    let dataEdit = (EntryList { regNumber= (regNumber profile), name = (name profile), gender = (gender profile), hc= (hc profile), flight= (flight profile)})
                                                    let _data = (dataList ++ [dataEdit])
                                                    let dataString = (_data >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")))
                                                    return dataString
                                    
changeData 2 argRegNumber arg = do
                                    let dataList = filter (\x-> not ((regNumber x) == argRegNumber)) arg
                                    let dataString = (dataList >>= (\x-> (show (regNumber x)++" "++(removeSpaceInput (name x))++" "++ (gender x)++" "++ show (hc x)++" "++(flight x)++ "\n")))
                                    return dataString
changeData _ _ [] = return("")

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
                putStrLn "---------------------------------------------------------"
                putStrLn "|No|Reg |Name\t\t\t|Gender\t|HC\t|Flight |"
                putStrLn "---------------------------------------------------------"
                let flightA = (getFlightList (convertTextToArray dataPlayer) "A")
                let flightB = (getFlightList (convertTextToArray dataPlayer) "B")
                let flightC = (getFlightList (convertTextToArray dataPlayer) "C")
                printEntryList 0 $ flightA
                putStrLn "---------------------------------------------------------"
                printEntryList (length flightA) $ flightB
                putStrLn "---------------------------------------------------------"
                printEntryList ((length flightB) + (length flightA)) $ flightC
                putStrLn "---------------------------------------------------------"
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

getFlightList :: [EntryList] -> String -> [EntryList]
getFlightList arg val = filter (\x-> (flight x) == val) arg

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
                    putStr "Your choose is : "
                    pilih <- getLine
                    case (pilih) of 
                        ("1") -> do
                                    putStrLn "=========== Draw Pairing List ==============="
                                    dataPlayer <- readFile "player.txt"
                                    putStrLn "Input minimum player per group :"
                                    minPlayer <- getLine
                                    let countMaxGroup = (length $ convertTextToArray dataPlayer) `div` (read minPlayer)   
                                    putStrLn "============================================="
                                    putStrLn ("1. Total Player \t\t: "++ show (length $ convertTextToArray dataPlayer)++ " person")
                                    putStrLn ("2. Player per group \t\t: "++ minPlayer ++ " person")
                                    putStrLn ("3. Total Group \t\t\t: "++ show (countMaxGroup))
                                    putStrLn "============================================="
                                    putStrLn "Press key `1` to continue draw. or any key to cancel"
                                    drawChoose <- getLine
                                    case (drawChoose) of 
                                        ("1") -> do
                                            -- Clear Directory -----
                                            directoryData <- listDirectory "."
                                            if("group" `elem` directoryData) then 
                                                --Check if folder is not empty, then delete all file
                                                do
                                                    putStrLn "Folder group is Exist"
                                                    filesGroup <- listDirectory "./group"
                                                    removeDirectoryWithFiles filesGroup
                                            else 
                                                -- if folder is not empty then create folder group
                                                do
                                                    putStrLn "Folder group is not Exist"
                                                    createDirectory "group"
                                            clearDrawData 0 countMaxGroup
                                            orderPlayer <- orderPlayerList dataPlayer
                                            drawData 0 countMaxGroup ( orderPlayer >>= (\x-> x ++ []) )  
                                            viewPairingList
                                            putStrLn "Press any key to continue...."
                                            getLine
                                            pairingList arg
                                        (_) -> pairingList arg
                        ("2") -> do
                                    viewPairingList
                                    putStrLn "Press any key to continue...."
                                    getLine
                                    pairingList arg
                        ("3") -> putStrLn "Back To Main Menu"
                        (_) -> pairingList arg

viewPairingList :: IO()
viewPairingList = do
                    putStrLn "=============== List Pairing ================="
                    dataPairing <- listDirectory "./group"
                    printPairingList 0 (sort dataPairing)


printPairingList :: Int -> [String] -> IO()
printPairingList _ [] = return ()
printPairingList index (x:xs) = do
                        dataPlayerString <- readFile "player.txt"
                        let dataPlayer = convertTextToArray dataPlayerString
                        dataPairingString <- readFile ("group/"++ x)
                        let dataPairing = words dataPairingString
                        putStrLn (removeTextExtentionFile x)
                        putStrLn ("---------------------------------------------------------")
                        putStrLn "|No|Reg |Name\t\t\t|Gender\t|HC\t|Flight |"
                        putStrLn ("---------------------------------------------------------")
                        let dataGroup = dataPairing >>= (\y-> filter (\x-> (regNumber x) == (read y)) dataPlayer)
                        tablePairing 0 dataGroup
                        putStrLn ("---------------------------------------------------------")
                        printPairingList (index+1) xs

tablePairing :: Int -> [EntryList] -> IO()
tablePairing _ [] = return()
tablePairing index (x:xs) = do 
                                let namePlayer = if (length (name x) < 9) then (name x) ++ "\t\t\t" 
                                                 else if (length (name x) >= 9  && length (name x) < 14) then (name x) ++ "\t\t"
                                                 else  (name x) ++ "\t"
                                putStrLn ("|0"++show (index+1)++"|"++ show (regNumber x) ++"|"++ namePlayer ++ "|"++ (gender x) ++ "\t|"++ (show (hc x))++"\t|"++ (flight x)++"\t|")
                                tablePairing (index+1) xs

randomIntList :: Int -> IO [Int]
randomIntList n = do
                     gen <- newStdGen
                     return (take n (randomRs (1000, 9999) gen))

createTupple :: [Int] -> [EntryList] -> [(Int,Int)]
createTupple [] [] = []
createTupple (x:xs) (y:ys) = [(x, (regNumber y))] ++ (createTupple xs ys) 

removeDirectoryWithFiles :: [String] -> IO()
removeDirectoryWithFiles [] = return()
removeDirectoryWithFiles (x:xs) = do
                                    removeFile ("./group/"++x)
                                    removeDirectoryWithFiles xs

orderPlayerList :: String -> IO [[Int]]
orderPlayerList " " = return []
orderPlayerList dataPlayer = 
    do

        let flightA = (getFlightList (convertTextToArray dataPlayer) "A")
        
        randomIntA <- randomIntList $ length flightA

        let orderFligthA = (sort $ createTupple randomIntA flightA) >>= (\(a,b)-> [b])
        let dataTypeOrderFlightA = orderFligthA >>= (\y-> filter (\x-> (regNumber x) == y) flightA) 

        let flightB = (getFlightList (convertTextToArray dataPlayer) "B")
        randomIntB <- randomIntList $ length flightB

        let orderFligthB = (sort $ createTupple randomIntB flightB) >>= (\(a,b)-> [b])
        let dataTypeOrderFlightB = orderFligthB >>= (\y-> filter (\x-> (regNumber x) == y) flightB) 

        let flightC = (getFlightList (convertTextToArray dataPlayer) "C")
        randomIntC <- randomIntList $ length flightC

        let orderFligthC = (sort $ createTupple randomIntC flightC) >>= (\(a,b)-> [b])
        let dataTypeOrderFlightC = orderFligthC >>= (\y-> filter (\x-> (regNumber x) == y) flightC) 

        return ([orderFligthA]++[orderFligthB]++[orderFligthC])


clearDrawData :: Int -> Int -> IO()
clearDrawData _ 0 = putStrLn "Selesai"
clearDrawData index group = do 
                                writeFile ("group/group"++(show (index+1))++".txt") ""  
                                clearDrawData (index+1) (group-1)

data FlightList = FlightList {groupNumber:: Int, player:: [EntryList] } deriving Show

drawData :: Int -> Int -> [Int] -> IO()
drawData _ 0 _= putStrLn "Data Tidak Ada"
drawData _ _ []= putStrLn "Selesai"
drawData index group (x:xs)= 
                            do 
                                appendFile ("group/group"++(show (index+1))++".txt")  ((show x)++" ")
                                if( (index+1) < group) then drawData (index+1) group xs
                                else drawData 0 group xs

---------------------------- End Pairing Menu -----------------------------
