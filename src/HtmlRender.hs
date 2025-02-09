module HtmlRender where

import Data.Text qualified as T
import MarkdownTypes
import HtmlTypes

--------------------------------------------------------------------------------
-- The RenderHtml Typeclass
--------------------------------------------------------------------------------

class RenderHtml a where
  renderHtml :: a -> Html

instance RenderHtml Document where
  renderHtml (Document blocks) = Html "div" [] (map renderHtml blocks)

instance RenderHtml Block where
  renderHtml (Paragraph inlines) =
    Html "p" [] (renderInlines inlines)
  renderHtml (Heading level inlines) =
    Html (T.pack ("h" ++ show level)) [] (renderInlines inlines)
  renderHtml ThematicBreak =
    Html "hr" [] []
  renderHtml (BlockQuote blocks) =
    Html "blockquote" [] (map renderHtml blocks)
  renderHtml (CodeBlock _ txt) =
    Html "pre" [] [ Html "code" [] [RawText (escapeHtml txt)] ]
  renderHtml (List listType items) =
    renderList listType items
  renderHtml (HTMLBlock txt) =
    RawText txt
  renderHtml (LRD _) = 
    RawText ""  -- Link reference definitions are not rendered directly.

instance RenderHtml Inline where
  renderHtml (Str txt) = RawText txt
  renderHtml (Emph inlines) = Html "em" [] (renderInlines inlines)
  renderHtml (Strong inlines) = Html "strong" [] (renderInlines inlines)
  renderHtml (Code txt) = Html "code" [] [RawText (escapeHtml txt)]
  renderHtml (Link inlines url mTitle) =
    Html "a" (("href", url) : titleAttr mTitle) (renderInlines inlines)
  renderHtml (Image inlines src mTitle) =
    Html "img"
      (("src", src) : ("alt", extractText inlines) : titleAttr mTitle) []
  renderHtml HardBreak = Html "br" [] []
  renderHtml SoftBreak = RawText " "
  renderHtml (RawHtml txt) = RawText txt

instance RenderHtml ListItem where
  renderHtml (ListItem _ blocks) = Html "li" [] (map renderHtml blocks)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

renderInlines :: [Inline] -> [Html]
renderInlines = map renderHtml

renderList :: ListType -> [ListItem] -> Html
renderList Bullet items =
  Html "ul" [] (map renderHtml items)
renderList (Ordered start' _) items =
  let attrs = if start' /= 1 then [("start", T.pack (show start'))] else []
  in Html "ol" attrs (map renderHtml items)

extractText :: [Inline] -> Text
extractText = T.concat . map go
  where
    go (Str t)       = t
    go (Emph xs)     = extractText xs
    go (Strong xs)   = extractText xs
    go (Code t)      = t
    go (Link xs _ _) = extractText xs
    go (Image xs _ _) = extractText xs
    go HardBreak     = " "
    go SoftBreak     = " "
    go (RawHtml t)   = t

titleAttr :: Maybe Text -> [(Text, Text)]
titleAttr Nothing  = []
titleAttr (Just t) = [("title", t)]

escapeHtml :: Text -> Text
escapeHtml = T.concatMap escapeChar
  where
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar '&'  = "&amp;"
    escapeChar '"'  = "&quot;"
    escapeChar c    = T.singleton c
