{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc,
      allFilesIn,
      traverseDirectory
    ) where

import System.Directory
import System.FilePath ((</>))
import Control.Monad
import Control.Exception
import Data.Maybe (fromMaybe)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

allFilesIn :: FilePath -> IO [FilePath]
allFilesIn = traverseDirectory id

traverseDirectory :: ([FilePath] -> [FilePath]) -> FilePath -> IO [FilePath]
traverseDirectory dirFilter dir = do
    (dirs, files) <- getDirectoryContents dir >>= filterM isFileReadable >>= splitDirsFiles
    let subdirs = (dirFilter . map ((</>) dir) . filter (`notElem` [".", ".."])) dirs
    fmap (concat . (files:)) $ forM subdirs (traverseDirectory dirFilter)
  where 
    splitDirsFiles :: [FilePath] -> IO ([FilePath],[FilePath])
    splitDirsFiles xs = do
      dirs <- filterM doesDirectoryExist xs
      files <- filterM doesFileExist xs
      return (dirs,files)
    isFileReadable :: FilePath -> IO Bool
    isFileReadable f = do
      perms <- maybeIO $ getPermissions f 
      return $ maybe False readable perms
    maybeIO :: IO a -> IO (Maybe a)
    maybeIO act = handle (\(_::SomeException) -> return Nothing) (Just `liftM` act)

