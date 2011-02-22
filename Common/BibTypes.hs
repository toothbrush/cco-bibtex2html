module Common.BibTypes where

    -- a bibtex file is just a list of entries:
    data BibTex = BibTex [String] [Entry]
                  deriving Show

    -- a bibtex entry:
    data Entry = Entry { entryType :: EntryType 
                       , reference :: Reference
                       , fields    :: [Field]
                       }
                   deriving Show

    -- an attibute/value pair
    data Field = Field String String -- Name and contents of field
                 deriving Show

    getKey :: Field -> String
    getKey (Field k _) = k

    getValue :: Field -> String
    getValue (Field _ v) = v

    maybegetKey :: Maybe Field -> Maybe String
    maybegetKey Nothing            = Nothing
    maybegetKey (Just (Field k _)) = Just k

    maybegetValue :: Maybe Field -> Maybe String
    maybegetValue  Nothing           = Nothing
    maybegetValue (Just (Field _ v)) = Just v

    type EntryType = String
    type Reference = String

    -- the type of the bibtex algebra. Used to fold over a bibtex library
    type BibTexAlgebra bibtex preamble entry = ([preamble] -> [entry] -> bibtex, 
                                                String -> preamble,
                                                EntryType -> Reference -> [Field] -> entry)

    -- the fold function
    foldBibTex :: BibTexAlgebra bibtex preamble entry -> BibTex -> bibtex
    foldBibTex (bib, pa, entry) = fBibTex where
       fBibTex (BibTex preamble lentries) = bib (map pa preamble) (map fEntry lentries)
       fEntry  (Entry spec ref attr)      = entry spec ref attr
    
    instance Eq Entry where
        (==) e1 e2 = reference e1 == reference e2

    lookupField :: String -> [Field] -> Maybe Field
    lookupField key []                 = Nothing
    lookupField key (f@(Field k v):fs) | key == k  = Just f
                                       | otherwise = lookupField key fs

    instance Ord Field where
        compare (Field k1 v1) (Field k2 v2) 
            | k1 == "author" = LT
            | k1 == "year"   = GT
            | k1 == "title" 
           && k2 == "author" = GT
            | k1 == "title"  = LT
            | otherwise      = EQ

    instance Eq Field where
        (==) (Field k1 v1) (Field k2 v2) = k1 == k2
