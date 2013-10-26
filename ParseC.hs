module ParseC where
import Language.C
import Language.C.Analysis
import Language.C.System.GCC   -- preprocessor used

main  = parseMyFile "test.c" >>= printMyAST
-- main2 = parseMyFile "test.c" >>= analyseAST >>= print . pretty

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     checkResult "[Parsing]" parse_result

printMyAST :: CTranslUnit -> IO ()
printMyAST ctu = (print . show) ctu

test = do 
  ast <- parseMyFile "test.c"
  (globals,warnings) <- (checkResult "[Analysis]" . runTrav_) (analyseAST ast)
  print . pretty $ globals

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return
