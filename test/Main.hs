module Main (main) where

import Test.Hspec hiding (example)
import Data.Aeson (eitherDecode, FromJSON (parseJSON))
import qualified Data.ByteString.Lazy as B
import Data.Text (Text, unpack)
import MarkdownParser (parseMarkdown) -- Adjust the import to your parser's module
import Text.Megaparsec (parse)
import Data.Aeson.Types (FromJSON, (.:))
import Data.Aeson (withObject)

-- Define a data type to match the structure of the CommonMark test cases
data TestCase = TestCase
  { example :: Int
  , markdown :: Text
  , html :: Text
  } deriving (Show)

instance FromJSON TestCase where
  parseJSON = withObject "TestCase" $ \v -> TestCase
    <$> v .: "example"
    <*> v .: "markdown"
    <*> v .: "html"

main :: IO ()
main = hspec $ do
  describe "MarkdownParser" $ do
    it "passes all CommonMark specification tests" $ do
      -- Load and decode the test suite
      testData <- B.readFile "test/spec.json"
      case eitherDecode testData of
        Left err -> expectationFailure $ "Failed to parse test data: " ++ err
        Right testCases -> mapM_ runTest (testCases :: [TestCase])

runTest :: TestCase -> Expectation
runTest TestCase{..} =
label $ between (char '`') (char '`') (M.some parseInline)
sati
() is
))
