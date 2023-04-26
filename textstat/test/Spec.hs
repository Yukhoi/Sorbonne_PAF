import Test.Hspec
import TextStatSpec as TE
import qualified Data.Text.IO as TIO
import System.IO

main :: IO ()
main = do

  -- Open the file for reading
  handle <- openFile "pg9645.txt" ReadMode
  
  -- Read the contents of the file as a string
  fileContents <- hGetContents handle
  
  -- Convert the string to Text
  let textContents = T.pack fileContents
  
  -- Close the file handle
  hClose handle

  hspec $ do
  
    TE.engineSpec textContents


