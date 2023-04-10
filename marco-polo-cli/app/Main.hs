{-This is a Marco Polo CLI, the contract should look like this:
--cabal run marco-polo-cli -- --help
--cabal run marco-polo-cli -- --hello "Marco"
-}

import           Options.Applicative

data Options = Options
  { optHello :: String
  }

options :: Parser Options
options = Options
  <$> strOption
      ( long "hello"
     <> metavar "TARGET"
     <> help "Target for the Marco Polo Greeting" )

main :: IO ()
main = do
  opts <- execParser (info (options <**> helper) idm)
  let name = optHello opts
  putStrLn (marcoPolo name)

marcoPolo :: String -> String
marcoPolo name =
  if name == "Marco" then "Polo"
  else "No, " ++ name


