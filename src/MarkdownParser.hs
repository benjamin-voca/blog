module MarkdownParser (parseMarkdown) where

import Data.Text (pack, strip)
import qualified Data.Text as T
import MarkdownTypes
import Text.Megaparsec hiding (State)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

parseMarkdown :: Text -> Either (ParseErrorBundle Text Void) Document
parseMarkdown input = runParser parseDocument "" input

parseDocument :: Parser Document
parseDocument = Document <$> M.many parseBlock
  where
    -- A simplified block parser that covers headings, paragraphs, lists, etc.
    parseBlock :: Parser Block
    parseBlock =  try parseHeading
              <|> try parseList
              <|> try parseThematicBreak
              <|> try parseBlockQuote
              <|> try parseCodeBlock
              <|> try linkReferenceDefinition
              <|> parseParagraph

    parseParagraph :: Parser Block
    parseParagraph = do
      inlines <- manyTill parseInline (void (string "\n\n") <|> M.eof)
      if null inlines
        then fail "empty paragraph"
        else return $ Paragraph inlines
    parseHeading :: Parser Block
    parseHeading = do
      numHashes <- length <$> M.some (char '#') <* space
      content <- M.many parseInline 
      return (Heading numHashes content)

    parseThematicBreak :: Parser Block
    parseThematicBreak =
      ThematicBreak <$ (char '\n' *> choice (map (string)["***", "---", "___", "* * *"]) <* char '\n')

    -- Revised parseBlockQuote: collect one or more lines that start with '>'
    parseBlockQuote :: Parser Block
    parseBlockQuote = do
      -- Parse one or more lines that begin with '>'
      l <- M.some $ do
        _ <- char '>'
        _ <- optional (M.some (char ' '))
        line <- takeWhileP (Just "blockquote content") (/= '\n')
        _ <- optional newline
        return line
      let content = T.unlines l
      -- Instead of recursively calling parseBlock (which includes blockquotes),
      -- we simply treat the collected text as a single paragraph inside the blockquote.
      return $ BlockQuote [Paragraph [Str (strip content)]]

    -- | Parser for a fenced code block.
    parseCodeBlock :: Parser Block
    parseCodeBlock = do
      _ <- string "```"
      -- Parse an optional info string (e.g. language), up to the end of the line.
      info <- optional (takeWhileP (Just "info string") (/= '\n'))
      _ <- eol
      -- Parse the code content until a closing fence is encountered.
      code <- manyTill anySingle (try (string "```"))
      return $ CodeBlock (Fenced (fmap strip info)) (pack code)

    -- | Parses a list block. It first tries an ordered list, then an unordered list.
    parseList :: Parser Block
    parseList = try parseOrderedList <|> parseUnorderedList

    parseUnorderedList :: Parser Block
    parseUnorderedList = do
      items <- M.some parseUnorderedListItem
      return $ List Bullet items

    parseUnorderedListItem :: Parser ListItem
    parseUnorderedListItem = do
      _ <- oneOf ("-+*" :: String)
      _ <- M.some (char ' ')
      firstBlock <- parseParagraph
      nestedBlocks <- M.many (try parseNestedBlock)
      return $ ListItem { isLoose = False, liBlocks = firstBlock : nestedBlocks }

    parseOrderedList :: Parser Block
    parseOrderedList = do
      (n, firstItem) <- parseOrderedListItem
      restItems <- M.many (try parseOrderedListItem)
      let items = firstItem : map snd restItems
      return $ List (Ordered n '.') items

    parseOrderedListItem :: Parser (Int, ListItem)
    parseOrderedListItem = do
      n <- L.decimal
      _ <- oneOf (".)" :: String)
      _ <- M.some (char ' ')
      firstBlock <- parseParagraph
      nestedBlocks <- M.many (try parseNestedBlock)
      return (n, ListItem { isLoose = False, liBlocks = firstBlock : nestedBlocks })

    parseNestedBlock :: Parser Block
    parseNestedBlock = do
      _ <- newline
      _ <- count 4 spaceChar
      parseBlock

    linkReferenceDefinition :: Parser Block
    linkReferenceDefinition = do
      _ <- char '['
      lab <- pack <$> manyTill anySingle (char ']')
      _ <- string ": "
      url <- pack <$> manyTill anySingle (space <|> eof)
      title <-
        optional ( between (char '"') (char '"') (pack <$> M.many anySingle)
                <|> between (char '\'') (char '\'') (pack <$> M.many anySingle)
                <|> between (char '(') (char ')') (pack <$> M.many anySingle)
                )
      return $ LRD (LinkReferenceDefinition lab url title)

    parseInline :: Parser Inline
    parseInline = parseCode
               <|> parseStrong
               <|> parseEmph
               <|> parseImage
               <|> parseLink
               <|> parseHardLineBreak
               <|> parseSoftLineBreak
               <|> parseStr
      where
        parseStr = Str . pack <$> M.some (satisfy isTextChar)
        parseEmph = Emph <$> between (string "*" <|> string "_") (string "*" <|> string "_") (M.some parseInline)
        parseStrong = Strong <$> between (string "**" <|> string "__") (string "**" <|> string "__") (M.some parseInline)
        parseCode =
          label "code span" $
            Code . strip . pack <$> between (char '`') (char '`') (M.some (satisfy (/= '`')))
        parseLink :: Parser Inline
        parseLink = do
          linkText <- between (char '[') (char ']') (M.many parseInline)
          (url, mtitle) <- between (char '(') (char ')')
                              (parseLinkDestAndTitle :: Parser (String, Maybe String))
          return (Link linkText (pack url) (fmap pack mtitle))

        parseImage :: Parser Inline
        parseImage = do
          linkText <- between (string "![") (char ']') (M.many parseInline)
          (url, mtitle) <- between (char '(') (char ')')
                              (parseLinkDestAndTitle :: Parser (String, Maybe String))
          return (Image linkText (pack url) (fmap pack mtitle))
        parseHardLineBreak =
          label "hard line break" $
            try (char '\\' *> eol *> pure HardBreak)
            <|> (count 2 spaceChar *> eol *> pure HardBreak)
        parseSoftLineBreak = label "soft line break" $ eol *> pure SoftBreak
        parseLinkDestAndTitle :: Parser (String, Maybe String)
        parseLinkDestAndTitle = do
          skipSpaces
          url <- parseUrl
          skipSpaces
          mtitle <- optional parseTitle
          skipSpaces
          return (url, mtitle)
        parseTitle = M.some (satisfy isTextChar)
        parseUrl   = M.some (satisfy isTextChar)
        isTextChar c = not (c `elem` ("*_[]()!`" :: String))
        skipSpaces = skipMany spaceChar
