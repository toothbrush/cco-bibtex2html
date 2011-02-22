-- | The decision has been made to represent a BibTeX file as a list of entries, along with possibly some preamble. 
module Common.BibTypes where

    -- | a bibtex file is just a list of entries, with a list of preamble-strings
    data BibTex = BibTex [String] [Entry]
                  deriving Show

    -- | a bibtex entry has a type, a reference (it's name), and a list of fields
    data Entry = Entry { entryType :: EntryType  -- ^ book, thesis, etc.
                       , reference :: Reference  -- ^ the name 
                       , fields    :: [Field]    -- ^ a list of key/value pairs
                       }
                   deriving Show

    -- | a field is an attibute/value pair
    data Field = Field String String -- Name and contents of field
                 deriving Show

    -- | returns the key part, given a Field
    getKey :: Field -> String
    getKey (Field k _) = k

    -- | returns the value part, given a Field
    getValue :: Field -> String
    getValue (Field _ v) = v

    -- | sometimes we want to get the key from a Maybe Field. 
    maybegetKey :: Maybe Field -> Maybe String
    maybegetKey Nothing            = Nothing
    maybegetKey (Just (Field k _)) = Just k

    -- | ...and sometimes we want to get the value from a Maybe Field. 
    maybegetValue :: Maybe Field -> Maybe String
    maybegetValue  Nothing           = Nothing
    maybegetValue (Just (Field _ v)) = Just v

    -- | the entry type is characterised by a string, for example "book"
    type EntryType = String
    -- | the reference is also just a string, such as "pierce02"
    type Reference = String

    -- | the type of the bibtex algebra. Used to fold over a bibtex library,
    -- like when we want to convert a BibTeX structure into HTML. 
    --
    -- This seems the most natural way to define possible conversions from BibTex to 
    -- other (possibly tree-like) formats, such as Html later on. 
    type BibTexAlgebra bibtex preamble entry = ([preamble] -> [entry] -> bibtex, 
                                                String -> preamble,
                                                EntryType -> Reference -> [Field] -> entry)

    -- | How to fold over a BibTeX tree. Used when converting to Html in Bib2HTML.Tool.
    foldBibTex :: BibTexAlgebra bibtex preamble entry -> BibTex -> bibtex
    foldBibTex (bib, pa, entry) = fBibTex where
       fBibTex (BibTex preamble lentries) = bib (map pa preamble) (map fEntry lentries)
       fEntry  (Entry spec ref attr)      = entry spec ref attr
    
    -- | We implement equality on entries. When their names are the same, we consider them equal.
    instance Eq Entry where
        (==) e1 e2 = reference e1 == reference e2

    -- | Given a key, find the corresponding Field in a list of Fields. If it can't be found, Nothing is returned. 
    lookupField :: String -> [Field] -> Maybe Field
    lookupField key []                 = Nothing
    lookupField key (f@(Field k v):fs) | key == k  = Just f
                                       | otherwise = lookupField key fs

    instance Ord Field where
        compare = compareF

    -- | Our implementation of ordering on Fields. This is how we make sure
    -- that author, then title, then the other fields, and finally year, are 
    -- displayed, regardless of how they are placed in the .bib file. This implementation
    -- allows us to simply run `sort` on a list of Fields. 
    compareF :: Field -> Field -> Ordering
    compareF (Field k1 v1) (Field k2 v2) 
        | k1 == "author" = LT
        | k1 == "year"   = GT
        | k1 == "title" 
       && k2 == "author" = GT
        | k1 == "title"  = LT
        | otherwise      = EQ

    -- | Fields are considered equal when their keys are the same.
    instance Eq Field where
        (==) (Field k1 v1) (Field k2 v2) = k1 == k2
