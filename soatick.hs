import Data.Time.Clock ( getCurrentTime, UTCTime(utctDay) )
import Data.Time.Calendar ( toGregorian )
import System.Directory ( doesFileExist )
import System.IO ( stderr, hPutStrLn )

statefn :: String
statefn = "soatick.state"

main :: IO ()
main = do
  hPutStrLn stderr "soatick (c) 2011-2020 Ben Clifford <benc@hawaga.org.uk>"
  a <- nextSerialFromState
  b <- todaysBaseSerial
  let o = a `max` b
  print o
  writeState o

nextSerialFromState :: IO Integer
nextSerialFromState = do
  exists <- doesFileExist statefn
  if exists then do
    c <- readFile statefn
    let d = (length c) `seq` (read c :: Integer)
    return (d+1)
   else return 0

writeState :: Show a => a -> IO ()
writeState s = writeFile statefn (show s)

todaysBaseSerial :: IO Integer
todaysBaseSerial = do
   now <- (getCurrentTime :: IO UTCTime)
   let (y,mi,di) = toGregorian (utctDay now)
   let (m,d) = (toInteger mi, toInteger di)
   return $ (((y*100 + m) * 100) + d) * 100

