{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MarkdownParser (parse) where

import Data.Text (pack)
import MarkdownTypes
import Text.Megaparsec hiding (State)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

parseInline :: Parser Inline
parseInline = parseStr
  where
    parseStr = Str . pack <$> M.some (satisfy isTextChar)
    parseEmph = Emph <$> between (string "*" <|> string "_") (string "*" <|> string "_") (M.some parseInline)
    parseStrong = Strong <$>  between (string "**" <|> string "__") (string "**" <|> string "__") (M.some parseInline)
    parseCode = error "unimplemented"
    parseLink = error "unimplemented"
    parseImage = error "unimplemented"
    parseHardBreak = error "unimplemented"
    parseSoftBreak = error "unimplemented"
    parseRawHtml = error "unimplemented"

isTextChar :: Char -> Bool
isTextChar c = not (c `elem` ("*_[]()!`" :: String))

