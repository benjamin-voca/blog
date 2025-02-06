module MarkdownTypes where

-- | The top-level document is a sequence of blocks.
data Document = Document [Block]
  deriving (Show, Eq)

-- | A block element represents structural parts of the document.
data Block
  = Paragraph [Inline]                   -- ^ A paragraph of inlines.
  | Heading Int [Inline]                 -- ^ A heading with level (1–6) and content.
  | ThematicBreak                        -- ^ A horizontal rule.
  | BlockQuote [Block]                   -- ^ A block quote containing other blocks.
  | CodeBlock CodeBlockType Text         -- ^ A code block; the first field distinguishes
                                         --   indented vs. fenced (with an optional info string).
  | List ListType [ListItem]             -- ^ An ordered or bullet list.
  | HTMLBlock Text                       -- ^ A block of raw HTML.
  | LinkReferenceDefinition Text Text (Maybe Text)
                                         -- ^ A link reference definition:
                                         --   label, destination, and optional title.
  deriving (Show, Eq)

-- | Distinguishes indented code blocks from fenced ones.
data CodeBlockType
  = Indented                           -- ^ An indented code block.
  | Fenced (Maybe Text)                -- ^ A fenced code block with an optional info string.
  deriving (Show, Eq)

-- | A list may be ordered or unordered.
data ListType
  = Bullet                             -- ^ An unordered (bullet) list.
  | Ordered { start :: Int             -- ^ The starting number.
            , delimiter :: Char        -- ^ The delimiter (e.g. '.' or ')').
            }
  deriving (Show, Eq)

-- | A list item is a container for blocks. The boolean flag indicates whether
-- the item is “loose” (contains a blank line, see spec examples) or “tight.”
data ListItem = ListItem { isLoose  :: Bool
                         , liBlocks :: [Block]
                         }
  deriving (Show, Eq)

-- | Inline elements represent text-level formatting.
data Inline
  = Str Text                           -- ^ A run of literal text.
  | Emph [Inline]                      -- ^ Emphasized text (e.g. wrapped in * or _).
  | Strong [Inline]                    -- ^ Strong (bold) text.
  | Code Text                          -- ^ Inline code spans.
  | Link [Inline] Text (Maybe Text)    -- ^ A link with text, destination URL, and optional title.
  | Image [Inline] Text (Maybe Text)   -- ^ An image with alt text, source URL, and optional title.
  | HardBreak                          -- ^ A hard line break (e.g. a trailing backslash).
  | SoftBreak                          -- ^ A soft line break (typically a newline in a paragraph).
  | RawHtml Text                       -- ^ Raw HTML inline.
  deriving (Show, Eq)
