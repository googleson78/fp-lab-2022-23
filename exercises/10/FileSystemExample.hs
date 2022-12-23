data FileSystem

--
-- cat file1 file2 file3
-- cat file1 file2 > outfile
-- cat > outfile

-- cat nonexistentfile
--
-- cat > outfile1 outfile2
--

-- cat file1 file2 > outfile
data CatArgs = MkCatArgs
  { filesToCat :: Maybe (NonEmpty String)
  , outputFile :: Maybe String
  }


parseCatArgs :: [String] -> Maybe CatArgs

main = do
  x <- getLine
  case parseCatArgs x of
    Nothing -> putStrLn "this is not vlaid input because ..."
    Just x -> cat ...


cat ::
  CatArgs ->
  FileSystem ->
  Either Error FileSystem
cat args fs = ..modify file system..

rm ::
  [String] ->
  FileSystem ->
  FileSystem
rm args fs = undefined
