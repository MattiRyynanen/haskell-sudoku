import Test.DocTest

docTestFiles :: [String]
docTestFiles = map (\n -> "src/Hudoku/" ++ n ++ ".hs") fs
    where fs = ["Definitions", "Snippets", "PerHouseSolvers"]

main :: IO ()
main = do
    print docTestFiles
    doctest $ "-isrc" : docTestFiles
