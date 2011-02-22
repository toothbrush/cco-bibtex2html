-- | This file is a place to store a table containing allowed BibTeX entry types,
-- and the required and optional fields for each. 
-- Adapted from <http://en.wikipedia.org/wiki/BibTeX>
module Common.AllowedFields where
    
    import Common.BibTypes

    -- | a table containing all the allowed BibTeX types and their attributes
    allowedTable :: [FieldTable]
    allowedTable = [ keysArticle
                   , keysBook
                   , keysBooklet
                   , keysConference
                   , keysInbook
                   , keysIncollection
                   , keysInproceedings
                   , keysManual
                   , keysMastersthesis
                   , keysMisc
                   , keysPhdthesis
                   , keysProceedings
                   , keysTechreport
                   , keysUnpublished
                   ]

    -- | An entry mapping from the BibTeX entry type to the list of required and optional keys
    type FieldTable = (EntryType, ([RequiredKey], [OptionalKey]))
    -- | A required key is simply defined by the string-representation of it's attribute name.
    type RequiredKey = String
    -- | Like required keys, optional keys are strings.
    type OptionalKey = String

    -- as an example, the @article type requires [author,title,journal,year], the rest of the keys mentioned are optional.
    keysArticle,keysBook,keysBooklet,keysConference,keysInbook,keysIncollection,keysInproceedings,keysManual,keysMastersthesis,keysMisc,keysPhdthesis,keysProceedings,keysTechreport,keysUnpublished :: FieldTable

    keysArticle       = ("article",
                        (["author","title","journal","year"],
                         ["volume","number","pages","month","note","key"]))
    keysBook          = ("book",
                        (["author","title","publisher","year"],
                         ["editor","volume","series","edition","month","note","key","pages","address"]))
    keysBooklet       = ("booklet",
                        (["title"],
                         ["author","howpublished","address","month","year","note","key"]))
    keysConference    = ("conference",
                        (["author","title","booktitle","year"],
                         ["editor","pages","organization","publisher","address","month","note","key"]))
    keysInbook        = ("inbook",
                        (["author","title","chapter","pages","publisher","year"],
                         ["editor","volume","series","address","edition","month","note","key","pages"]))
    keysIncollection  = ("incollection",
                        (["author","title","booktitle","year"],
                         ["editor","pages","organization","publisher","address","month","note","key"]))
    keysInproceedings = ("inproceedings",
                        (["author","title","booktitle","year"],
                         ["editor","series","pages","organization","publisher","address","month","note","key"]))
    keysManual        = ("manual",
                        (["title"],
                         ["author","organization","address","edition","month","year","note","key"]))
    keysMastersthesis = ("mastersthesis",
                        (["author","title","school","year"],
                         ["address","month","note","key"]))
    keysMisc          = ("misc",
                        ([],
                         ["author","title","howpublished","month","year","note","key","url"]))
    keysPhdthesis     = ("phdthesis",
                        (["author","title","school","year"],
                         ["address","month","note","key"]))
    keysProceedings   = ("proceedings",
                        (["title","year"],
                         ["editor","publisher","organization","address","month","note","key"]))
    keysTechreport    = ("techreport",
                        (["author","title","institution","year"],
                         ["typef","number","address","month","note","key"]))
    keysUnpublished   = ("unpublished",
                        (["author","title","note"],
                         ["month","year","key"]))
    
