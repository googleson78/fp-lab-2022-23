-- Maybe Int
-- []

-- IO Int

-- getLine :: IO String

forever :: IO a -> IO a
forever mx = do
  mx
  forever mx

readThreeAndPrintThree :: IO ()
readThreeAndPrintThree = do
  (x :: String) <- (getLine :: IO String)

  y <- getLine

  z <- getLine

  putStrLn x
  putStrLn y
  putStrLn z

main :: IO ()
main = forever readThreeAndPrintThree
