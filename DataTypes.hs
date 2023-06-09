module DataTypes where
 type Node = (String, String, String)  

 --returns if the elements is directory or a file
 getFileType (fileType,_,_) 
      | fileType == "Folder" || fileType == "folder"  = "Folder"
      | fileType == "File" || fileType == "file"  = "File"
      | otherwise                           = "Error"
 --return the name 
 getFileName (_,fileName,_)         = fileName
 --return the content
 getFileContent (_,_,fileContent)   = fileContent 

 -- checks if is File 
 isFile file = getFileType file == "File" 
 -- checks if is Directory 
 isDirectory file = getFileType file == "Folder"
 --the tree which represents the file system 

 getType :: FileSystemTree -> String
 getType (File _ _) = "File"
 getType EmptyTree = "EmptyTree"
 getType (Folder _ _) = "Folder"
 -- the structure that will represent out file system
 data FileSystemTree  = EmptyTree 
                      | Folder  String [FileSystemTree]
                      | File    String String
                 deriving Show

 -- return the name of the file or folder
 getName (Folder name _) = name
 getName (File name _)   = name

 getContent (File _ content) = content
 getContent (Folder _ _ )    = "Folders have no content!"
 getContent EmptyTree        = "There is no file!"
