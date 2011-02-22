module Common.BibTypes where

    -- a bibtex file is just a list of entries:
    data BibTex = BibTex [Entry]
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

    type EntryType = String
    type Reference = String

    -- the type of the bibtex algebra. Used to fold over a bibtex library
    type BibTexAlgebra bibtex entry = ([entry] -> bibtex, 
                                       EntryType -> Reference -> [Field] -> entry)

    -- the fold function
    foldBibTex :: BibTexAlgebra bibtex entry -> BibTex -> bibtex
    foldBibTex (bib, entry) = fBibTex where
       fBibTex (BibTex lentries)     =  bib $ map fEntry lentries
       fEntry  (Entry spec ref attr) =  entry spec ref attr
    
    instance Eq Entry where
        (==) e1 e2 = reference e1 == reference e2

    lookupField :: String -> [Field] -> Maybe Field
    lookupField key []                 = Nothing
    lookupField key (f@(Field k v):fs) | key == k  = Just f
                                       | otherwise = lookupField key fs

    instance Ord Field where
        compare (Field k1 v1) (Field k2 v2) = compare v1 v2

    instance Eq Field where
        (==) (Field k1 v1) (Field k2 v2) = k1 == k2 && v1 == v2
