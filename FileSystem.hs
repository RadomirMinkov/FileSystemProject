import System.IO
import CommandHelpers
import DataTypes
import ConvertingFunctions
-- this is the case where we have an output file and files from which to get the text
copyTreeWithConcatinatedFileWhenFileExists :: [String] -> String -> FileSystemTree -> FileSystemTree
copyTreeWithConcatinatedFileWhenFileExists [] _ tree = tree
copyTreeWithConcatinatedFileWhenFileExists path newContent file@(File name content) = if length path == 1 && head path == name then File name (content ++ newContent) else file
copyTreeWithConcatinatedFileWhenFileExists path newContent tree@(Folder name subFiles) = if head path /= name  then tree else Folder name (foldr op [] subFiles)
  where op x y = copyTreeWithConcatinatedFileWhenFileExists (tail path) newContent x : y
-- concatinating content of the given files
concatFiles path textToConcat pwd tree = do
   if  existIfFullOrRelativePath pwd tree path then
                 if head path == '/'
                 then readingInput pwd $ copyTreeWithConcatinatedFileWhenFileExists (pathStringToList '/' path) textToConcat tree
                 else do
                  readingInput pwd $ copyTreeWithConcatinatedFileWhenFileExists (pathStringToList '/' (pwd  ++ path)) textToConcat tree
   else do
      readingInput pwd (addFileToSystem (pathStringToList '/' pwd) realPath ("File", last (pathStringToList '/' path), textToConcat) tree)
       where realPath 
              | head path == '/' = res
              | otherwise        = drop 1 res
              where res = pathListToString $ take (length lst - 1) lst
                    lst = pathStringToList '/' path 

proccessCommandCat :: [String] -> String -> FileSystemTree -> IO()
proccessCommandCat command pwd tree = do
                if null command || (length command == 1 && head command == ">" )then do
                                output <- readWhile ""
                                putStrLn output
                                readingInput pwd tree
                else result
                   where result
                           |  not $ existElemVector ">" command = do
                                                               putStrLn concatContent
                                                               readingInput pwd tree 
                           |  not $ null filesToReadInformationFrom   = concatFiles (last command) concatContent pwd tree
                           |  otherwise                         =  do
                                                           output <- readWhile ""
                                                           concatFiles (last command) output pwd tree
                           where concatContent = foldl op "" newLst
                                 newLst = takeWhile (/=">") command 
                                 op res curr = if null toAdd then res else res ++ " " ++ toAdd        
                                    where toAdd =  fileContent (pathStringToList '/' pwd) (pathStringToList '/' curr) tree
                                 filesToReadInformationFrom = takeWhile (/=">") command

--tests
-- 📂🗁🗐📄📝 🗎🗋🗎🎫
testTree = Folder "Entertainment" [Folder "Games" [Folder "PS" [File "GodOfWar" "Blades of chaos", File "TheLastOfUs" "virus"]],
                            File "University" "I love programming",
                            Folder "Football" [File "Liverpool" "best club",
                                               File "PremierLeague" "best league in the world"],
                            Folder "FilmsAndAnimes" [File "Naruto" "perfection",
                                                     File "AttackOnTitan" "also perfection",
                                                     File "GameOfThrones" "one of the best ever"],
                            File "Football" "It is the best sport"]
-- list of function that proccess the given command
printCommandHelp pwd tree = do
                  putStrLn "command help: help -> print the information below"
                  putStrLn "command pwd: pwd"
                  putStrLn "command add: add element where element is Node (File or Folder , name , if file content or if direcotry empty string) and give the path"
                  putStrLn "command cd: cd ..  or  cd /full/path   or cd relative/path"
                  putStrLn "command ls: ls  or ls /full/path   or ls  relative/path"
                  putStr   "command cat: cat <file1> <file2> ... <file n> > <exit> where files are full or relative path and if not file exist the input is taken from the standart input "
                  putStrLn "and the end to the command is a comma and if there is no exit the output is printed to the console. The both can be abscent"   
                  putStrLn "command removeAll: rmAll <file1> <file2> ... <file n>"
                  putStrLn "command removeFiles: rmFiles <file1> <file2> ... <file n>"
                  putStrLn "command removeFolders: rmFolders <file1> <file2> .... <file n>"
                  putStrLn "command print: print -> this command prints the whole system"
                  putStrLn "command exit: exit -> exiting the program"
                  readingInput pwd tree

proccessingChangeDirectory command pwd tree 
   | null command         = readingInput pwd tree
   | head command == ".." = readingInput (changeDirectory pwd ".." tree) tree 
   | otherwise            = readingInput (changeDirectory pwd (head command) tree) tree
                                          
readingInput pwd tree = do
           putStr "Enter a command: "
           x <- getLine 
           let command = pathStringToList ' ' x
           proccessingCommands command pwd tree

proccessingPrintDirectory command pwd tree = do
          putStrLn $ case command of
            [] -> printDirectory [] pwd tree
            ("/":xs) ->  "\x1b[32m" ++ printDirectory (pathStringToList '/' (head command)) "" tree
            (x : xs) ->  "\x1b[32m" ++ printDirectory (pathStringToList '/' pwd) (head command) tree
          putStrLn $ "\x1b[32m" ++ "test" ++ "\x1b[0m"
          readingInput pwd tree


test = do
   let x =  "\x1b[32m " ++ "test"
   putStrLn x
proccessingCurrentD pwd tree = do
       putStrLn pwd
       readingInput pwd tree
commandPrint pwd tree = do 
       putStrLn "-------------------------------"
       putStr $ printTree 0 tree
       putStrLn "-------------------------------"
       readingInput pwd tree


proccessCommandExit  tree = do
               putStrLn "Bye! Have a great day!"
               testFile <- openFile "FileSystemPrototype.txt" WriteMode
               if getType tree == "EmptyTree" then hClose testFile
               else do
                  hPutStr testFile $ printToFile 1 tree
                  hClose testFile

-- function that proccess the given command 
proccessingCommands command pwd tree= do
  case head command of
   "help"          -> printCommandHelp pwd tree
   "exit"          -> proccessCommandExit tree
   "cd"            ->  proccessingChangeDirectory (tail command) pwd tree
   "ls"            -> proccessingPrintDirectory (tail command) pwd tree
   "pwd"           -> proccessingCurrentD pwd tree
   "print"         -> commandPrint pwd tree
   "cat"           -> proccessCommandCat (tail command) pwd tree
   "removeAll"     -> readingInput pwd $ filterNonEmptyFiles (removeAllFromSystem (tail command) (pathStringToList '/' pwd) tree)
   "removeFiles"   -> readingInput pwd $ filterNonEmptyFiles (removeOnlyFiles (tail command) (pathStringToList '/' pwd) tree)
   "removeFolders" -> readingInput pwd $ filterNonEmptyFiles (removeOnlyFolders (tail command) (pathStringToList '/' pwd) tree)
   "add"           -> readingInput pwd $ addFileToSystem (pathStringToList '/' pwd) (head (tail command))  (makeFile (tail (tail command))) tree
   _               -> readingInput pwd tree

printToFile :: Int -> FileSystemTree -> String
printToFile level (Folder name children) = printRepeat level ++ "Folder " ++ name ++ "\n" ++ foldr op "" children
   where op x y = printToFile (level + 1) x ++ y
printToFile level (File name content) = printRepeat level ++ "File " ++ name ++ " " ++ content ++ "\n"

readFiles file path level tree line mainFile= do
      let stringList = pathStringToList ' ' line
      let intervalsCount = length $ takeWhile (=="") stringList
      let realInput =  dropWhile (=="") stringList
      let fileToAdd = makeFile realInput
      let newTree = addFileToSystem [] path fileToAdd tree
      endFile <- hIsEOF file
      if endFile then  do
         hClose mainFile
         readingInput "/" newTree
      else do
         nextLine <- hGetLine file
         let intervalsCountNextLine = length $ takeWhile (=="") $ pathStringToList ' ' nextLine
         if intervalsCount <  intervalsCountNextLine 
         then readFiles file (path ++ "/" ++ getFileName fileToAdd) intervalsCount newTree nextLine mainFile
         else readFiles file (pathListToString (take intervalsCountNextLine (pathStringToList '/' path))) intervalsCount newTree nextLine mainFile
readFromFile = do 
    mainFile <- openFile "FileSystemPrototype.txt" ReadMode
    endFile <- hIsEOF mainFile
    if endFile then do
              putStrLn "The file is empty!"
              printCommandHelp "/" EmptyTree
    else do
     input <- hGetLine mainFile
     let listInput = pathStringToList ' ' input
     nextLine <- hGetLine mainFile
     readFiles mainFile ("/" ++ getFileName (makeFile listInput)) 0  (addFileToSystem [] [] (makeFile listInput) EmptyTree) nextLine mainFile

main :: IO() 
main = do
    putStrLn  "Hello user!"
    let pwd = "/"
    putStrLn "Do you want to load a file system from the FileSystemPrototype.txt file?"
    answer <- getLine
    if answer == "Yes" then readFromFile else  printCommandHelp pwd EmptyTree 
