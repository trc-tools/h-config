{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.Text
import Data.Yaml as Y
import Data.Aeson
    ( eitherDecode, (.:), FromJSON(parseJSON), Value(Object) )
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as Y
import System.FilePath.Posix as P

data Profile = 
    Profile {
    name :: !Text,
    favoriteGame :: !Text,
    subscriber :: Bool
    } deriving Show

instance FromJSON Profile where
 parseJSON (Object v) =
    Profile <$> v .: "name"
           <*> v .: "favorite-game"
           <*> v .: "subscriber"
 parseJSON _ = mzero    

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Missing argument to JSON file"
        [filepath] -> do
            validationResult <- validate filepath
            if validationResult
                then
                    do 
                        jsonE <- parse filepath
                        case jsonE of  
                            Left err -> error err
                            Right success -> print success
                else
                    putStrLn $ "File " ++ filepath ++ " does not exist"
        _ -> putStrLn "Too many arguments. Only 1 argument expected."


parse :: FilePath -> IO (Either String [Profile])
parse fp = 
    let fileContent = B.readFile fp in
        case takeExtension fp of 
        ".yml" -> Y.eitherDecode  <$> fileContent
        ".json" -> eitherDecode <$> fileContent
        _ -> return $ Left "Unknown extension"

validate :: FilePath -> IO Bool
validate = doesFileExist
