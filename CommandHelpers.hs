 module CommandHelpers where
 import DataTypes
 import ConvertingFunctions
 -- checks if the nodes without the content are equal
 equalNodes :: Node -> Node -> Bool
 equalNodes nodeOne nodeTwo = getFileType nodeOne == getFileType nodeTwo && getFileName nodeOne == getFileName nodeTwo
 -- checks if the nodes are equal and if the content inside the files is thge same
 sameFiles :: Node -> Node -> Bool
 sameFiles nodeOne nodeTwo  = equalNodes nodeOne nodeTwo && getFileContent nodeOne == getFileContent nodeTwo

 -- checks if elem exists in the whole system
 existElem :: String -> FileSystemTree -> Bool
 existElem  _  EmptyTree  = False
 existElem file (Folder name children) = file == name || foldr op False children
   where op x y = existElem file x || y
 existElem file (File name _)  = file == name
 -- checks if file exists in directory
 existElemDirectory :: Node -> FileSystemTree -> Bool
 existElemDirectory elem EmptyTree = False
 existElemDirectory elem tree@(File name content) = False
 existElemDirectory elem tree@(Folder name subFiles) = foldr op False subFiles
  where op x y = (getFileType elem == getType x && getFileName elem == getName x )|| y

 -- helper function for printing the tree
 printRepeat 0     =  ""
 printRepeat times =  " " ++ printRepeat  (times - 1)
-- printing the system 
 printTree :: Int -> FileSystemTree -> String
 printTree _ EmptyTree  = ""
 printTree level (Folder name children) =  printRepeat level ++ "Folder: " ++ name ++"\n" ++ foldr op "" children
   where op x y = printTree (level + 5) x ++ y
 printTree level (File name content)    =  printRepeat level ++ "File: " ++ name ++ "\n"

 -- searching element in a tree and if found returning the whole subtree with a root the searched element
 searchElem :: String -> FileSystemTree -> [FileSystemTree]
 searchElem elem EmptyTree = []
 searchElem elem tree@(Folder name children) = if elem == name then [tree] else foldr op [] children
   where op x y = searchElem elem x
 searchElem elem tree@(File name _)  = [tree | elem == name]
 -- returning path to searched file
 pathToFile :: String -> FileSystemTree -> String
 pathToFile _ EmptyTree = ""
 pathToFile file tree@(Folder name children)
   | not (existElem file tree) = "" 
   |  file == name              = "" 
   |otherwise                   = name ++ "/" ++ foldr op "" children
   where op x y =  if null newStr then y else newStr 
           where newStr = pathToFile file x
 pathToFile file (File name _)  = ""

 -- checking if file is valid
 isValid file = isFile file|| isDirectory file

 --checks if the path is valid
 isValidPath :: [String] -> FileSystemTree -> Bool
 isValidPath [] _         = True 
 isValidPath _  EmptyTree = False
 isValidPath path (Folder name children) = head path == name && (null children || foldr op False children)
   where op x y 
          |  null (tail path)  = isValidPath (tail path) x
          |  otherwise         = if head (tail path) == getName x then isValidPath (tail path) x || y  else  y
 isValidPath path (File name _)  = length path == 1 && head path == name 

 --checks if the path to a file is valid
 isValidPathToFile :: [String] -> FileSystemTree -> Bool
 isValidPathToFile [] (Folder _ _ )         = False
 isValidPathToFile [] (File _ _)   = True 
 isValidPathToFile _  EmptyTree = False
 isValidPathToFile path (Folder name children) = head path == name &&  length path /= 1 && foldr op False children
   where op x y 
          |  null (tail path)  = isValidPathToFile (tail path) x
          |  otherwise         = if head (tail path) == getName x then isValidPathToFile (tail path) x || y  else  y
 isValidPathToFile path (File name _)  = length path == 1 && head path == name 

 changeDirectory ::String -> String -> FileSystemTree -> String 
 changeDirectory currentD "" _ = currentD
 changeDirectory "/" ".." _    = "/"
 changeDirectory currentD ".." _ = pathListToString (take (length newString - 1) newString) ++ "/"
  where newString = pathStringToList '/' currentD  
 changeDirectory currentD path tree = if last res == '/' then res else res ++ "/"
          where res = if head path == '/' then  fullValidPath  else relativeValidPath
                fullValidPath = if validPath then path else currentD
                  where validPath = isValidPath (pathStringToList '/' path) tree
                relativeValidPath = if validPath then relativePath else currentD
                  where validPath    = isValidPath (pathStringToList '/' relativePath) tree
                        relativePath =  currentD  ++ path
 -- adding file
 addFileToSystem :: [String] -> String -> Node ->FileSystemTree ->FileSystemTree
 addFileToSystem [] "" elToAdd EmptyTree
              | isFile elToAdd      = File (getFileName elToAdd) (getFileContent elToAdd)
              | isDirectory elToAdd = Folder (getFileName elToAdd) []
              | otherwise           = EmptyTree
 addFileToSystem [] "" elToAdd tree@(Folder name files) 
              | isFile elToAdd  && notExists     = Folder name (files ++ [File (getFileName elToAdd) (getFileContent elToAdd)])
              | isDirectory elToAdd && notExists = Folder name (files ++ [Folder (getFileName elToAdd) []])
              | otherwise           = tree
              where notExists = not $ existElemDirectory elToAdd tree 
 addFileToSystem _ "" elToAdd tree@(File name content) = tree
 addFileToSystem _  "" elToAdd EmptyTree  = EmptyTree
 addFileToSystem pwd "" elToAdd tree@(Folder name files) = if head pwd == name then Folder name newFiles else tree
   where newFiles
          | exists   = files
          | null files      = newFile
          | null (tail pwd)  = files ++ newFile
          | otherwise       = foldr op [] files
          where newFile 
                 | isFile elToAdd      = [File (getFileName elToAdd) (getFileContent elToAdd)]
                 | isDirectory elToAdd = [Folder (getFileName elToAdd) []]
                 | otherwise           = []
                op x y  = addFileToSystem (tail pwd) "" elToAdd x : y
                --wrong directory testing
                exists = existElemDirectory elToAdd tree
 addFileToSystem [] "/"  elToAdd EmptyTree = addFileToSystem (pathStringToList '/' "/") "" elToAdd EmptyTree
 addFileToSystem [] path elToAdd EmptyTree = EmptyTree
 addFileToSystem [] path elToAdd tree@(File name content) = tree
 addFileToSystem [] path elToAdd tree@(Folder name files)     = addFileToSystem (pathStringToList '/' path) "" elToAdd tree
 addFileToSystem pwd path elToAdd EmptyTree = EmptyTree
 addFileToSystem pwd path elToAdd tree@(File name content) = tree
 addFileToSystem pwd path elToAdd tree@(Folder name files) = if head path == '/' then addFullPath else addRelativePath
   where addFullPath     = addFileToSystem (pathStringToList '/' path) "" elToAdd tree
         addRelativePath = if head pwd /= name then tree else Folder name newBranches
            where newBranches = foldr op [] files
                   where op x y = addFileToSystem (tail pwd) path elToAdd x : y

 --remove single file 
 removeFile :: [String] -> String ->FileSystemTree -> FileSystemTree
 removeFile _ _ EmptyTree   = EmptyTree
 removeFile _ "" tree        = tree
 removeFile [] fileName tree@(File name content) = if fileName == name then EmptyTree else tree
 removeFile pwd _ tree@(File name content) = tree
 removeFile [] fileName tree@(Folder name children) = tree 
 removeFile pwd fileName tree@(Folder name children) = if head pwd /= name then tree else Folder name (foldr op [] children)
  where op x y = removeFile (tail pwd) fileName x : y

 --remove a single folder
 removeFolder :: [String] -> String -> FileSystemTree -> FileSystemTree
 removeFolder _ _ EmptyTree = EmptyTree
 removeFolder _ "" tree      = tree
 removeFolder _ _ tree@(File name content) = tree
 removeFolder [] fileName tree@(Folder name children)  = if fileName == name then EmptyTree else tree
 removeFolder pwd fileName tree@(Folder name children) = if head pwd /= name then tree else Folder name (foldr op [] children)
  where op x y = removeFolder (tail pwd) fileName x : y  

 --removing all files and folder in the direcotyr which have the same name as one of the given files/folder
 removeAllFromSystem :: [String] -> [String] -> FileSystemTree ->FileSystemTree
 removeAllFromSystem _ _ EmptyTree = EmptyTree
 removeAllFromSystem [] _ tree     = tree
 removeAllFromSystem files [] tree = foldr op tree files
   where op x y = removeFile [] x (removeFolder [] x y)
 removeAllFromSystem files pwd file@(File name content) = file
 removeAllFromSystem files pwd tree@(Folder name subFiles)  = if head pwd /= name then tree else Folder name (foldr op [] subFiles)
  where op x y = removeAllFromSystem files (tail pwd) x : y

 -- remove only files
 removeOnlyFiles :: [String] -> [String] -> FileSystemTree -> FileSystemTree
 removeOnlyFiles _ _ EmptyTree = EmptyTree
 removeOnlyFiles [] _ tree = tree
 removeOnlyFiles files [] tree =foldr op tree files
   where op = removeFile [] 
 removeOnlyFiles files pwd tree@(File name content) = tree
 removeOnlyFiles files pwd tree@(Folder name subFiles) = if head pwd /= name then tree else Folder name (foldr op [] subFiles)
   where op x y = removeOnlyFiles files (tail pwd) x : y

 -- remove only folders
 removeOnlyFolders :: [String] -> [String] -> FileSystemTree -> FileSystemTree
 removeOnlyFolders _ _ EmptyTree = EmptyTree
 removeOnlyFolders _ _ file@(File name content) = file
 removeOnlyFolders [] _ folder@(Folder name content) = folder
 removeOnlyFolders files [] tree = foldr op tree files
  where op  = removeFolder [] 
 removeOnlyFolders files pwd tree@(Folder name subFiles) = if head pwd /= name then tree else Folder name (foldr op [] subFiles)
  where op x y = removeOnlyFolders files (tail pwd) x : y

 -- filter nonEmpty files, remove all the emptyTree elements in the system
 filterNonEmptyFiles :: FileSystemTree -> FileSystemTree
 filterNonEmptyFiles EmptyTree = EmptyTree
 filterNonEmptyFiles file@(File _ _) = file
 filterNonEmptyFiles (Folder name subFiles) = Folder name (foldr op [] subFiles)
   where op x y 
          | getType x == "Folder" = filterNonEmptyFiles x : y
          | getType x == "File"   = x : y
          | otherwise               =  y

 -- print given directory 
 printDirectory :: [String] -> String -> FileSystemTree -> String
 printDirectory _ _ EmptyTree = "There is no such directory!"
 printDirectory [] "" (File name content)    =  "\x1b[32m" ++  name 
 printDirectory [] "" (Folder name subFiles) =   "\x1b[32m" ++ name
 printDirectory pwd "" tree@(Folder name subFiles) = if head pwd /= name then "" else result
  where result = if null printingString then "The directory is empty!" else printingString
        printingString = foldr op "" subFiles
        op x y 
          | null $ tail pwd = foldr printOp "" subFiles
          | otherwise       = printDirectory (tail pwd) "" x ++ y
         where printOp curr res = getName curr ++ " " ++ res
 printDirectory pwd "" tree@(File name content) = ""
 printDirectory [] path tree@(Folder name subFiles) = printDirectory (pathStringToList '/' path) "" tree
 printDirectory _ _ tree@(File name content) = ""
 printDirectory pwd path tree@(Folder name subFiles) = if head path == '/' then printDirectory (pathStringToList '/' path) "" tree else printRelativePath
   where printRelativePath = if head pwd /= name then "Wrong path"else foldr op "" subFiles
         op x y = printDirectory (tail pwd) path x ++ y

 -- get the content of the file to a given path
 fileContent :: [String] -> [String] -> FileSystemTree -> String
 fileContent  _ _ EmptyTree = ""
 fileContent [] [] tree@(File name content)            = "" 
 fileContent [] [] tree@(Folder name subFiles)         = ""
 fileContent currentD [] (File name content)           = if length currentD == 1 && head currentD == name then content  else ""
 fileContent currentD [] tree@(Folder name subFiles)   = if head currentD /= name then "" else foldr op "" subFiles
  where op x y = fileContent (tail currentD) [] x ++ y
 fileContent [] path tree                              = fileContent path [] tree
 fileContent  currentD path (File name content)        = ""
 fileContent currentD path tree@(Folder name subFiles) = if head currentD /= name then "" else foldr op "" subFiles
   where op x y = fileContent (tail currentD) path x ++ y

 -- checks if an element exist in list
 existElemVector :: String -> [String] -> Bool
 existElemVector elem [] = False
 existElemVector elem list = foldr op False list
   where op x y = (x == elem) || (y || False)

 -- reading user input from the console until only a comma is inputed on a separate line
 readWhile res = do
      input <- getLine
      if input == "." then return res else readWhile (res ++ input)
 existIfFullOrRelativePath :: String -> FileSystemTree -> String -> Bool
 -- the functions checks if we have valid path to file no matter if it is full or relative
 existIfFullOrRelativePath pwd tree path 
    | head path == '/'   = isValidPathToFile (pathStringToList '/' path) tree
    | otherwise          = isValidPathToFile  (pathStringToList '/' (pwd ++ path)) tree
 
