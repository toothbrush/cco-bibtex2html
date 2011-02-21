module Common.Types where

    -- a bibtex file is just a list of entries:
    data BibTex = BibTex [Entry]
                  deriving Show

    -- a bibtex entry:
    data Entry = Entry { entryType :: EntryType 
                       , reference :: String
                       , fields    :: [Field]
                       }
                   deriving Show

    data Field = Field String String -- Name and contents of field
                 deriving Show

    type EntryType = String  -- we'll validate this later. 
