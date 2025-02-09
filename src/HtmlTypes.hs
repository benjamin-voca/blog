module HtmlTypes where

import qualified Data.Text as T
import GHC.Show (Show(..))

data Html = Html
  { tagName    :: Text
  , attributes :: [(Text, Text)]
  , children   :: [Html]
  }
  | RawText Text
   deriving (Eq)

instance Show Html where
  show (RawText t) = T.unpack t
  show Html{..} =
    let attrsStr = if null attributes then "" else " " <> T.unpack (T.unwords (map showAttr attributes))
        childrenStr = concatMap GHC.Show.show children
    in "<" <> T.unpack tagName <> attrsStr <> ">" <> childrenStr <> "</" <> T.unpack tagName <> ">"
    where
      showAttr (key, value) = key <> "=\"" <> value <> "\""
