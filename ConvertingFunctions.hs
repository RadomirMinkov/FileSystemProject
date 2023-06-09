module ConvertingFunctions where
 import DataTypes
 -- list to string
 listToString :: [String] -> String
 listToString [] = ""
 listToString lst = foldr op "" lst
  where op x y = x ++ " " ++ y
 -- make file
 makeFile :: [String] -> Node
 makeFile lst 
     | (head lst == "File" || head lst == "file")  &&  length lst == 3 = (head lst , head (tail lst), head(tail (tail lst)))
     | (head lst == "Folder" || head lst == "folder")  &&  length lst == 2 = (head lst , head (tail lst), "")
     | (head lst == "File" || head lst == "folder")  &&  length lst > 3 = (head lst,head (tail lst), listToString (tail (tail lst)))
     |otherwise                                                = ("Error", "Error", "Error")
 -- converting the given path from a string to list of strings
 pathStringToList :: Char -> String -> [String]
 pathStringToList _ ""      = []
 pathStringToList splitter path 
          | head path == splitter && length path == 1 = []
          | head path == splitter = takeWhile (/= splitter) (tail path) : pathStringToList splitter newStringOne 
          | otherwise             = takeWhile (/= splitter) path : pathStringToList splitter newStringTwo
          where newStringOne = dropWhile (/= splitter) (tail path)
                newStringTwo = dropWhile (/= splitter) path

 --converting the given path in list of string to a string
 pathListToString :: [String] -> String
 pathListToString  [] = ""
 pathListToString  path = foldr op "" path
    where op x y = "/" ++ x ++ y