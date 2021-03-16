import Test.Hspec ( hspec )
import Control.Exception (evaluate)
import TestsDisplay ( displaySpec )
import TestsLexer ( lexerSpec )
import TestsRules ( rulesSpec )
import TestsParser ( parserSpec )
import TestsGeneration ( generationSpec )

main :: IO ()
main = hspec $ do
  displaySpec 
  lexerSpec
  rulesSpec
  parserSpec
  generationSpec