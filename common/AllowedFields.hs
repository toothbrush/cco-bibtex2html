module Common.AllowedFields where
    
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

    keysArticle       = ("article",
                        (["author","title","journal","year"],
                         ["volume","number","pages","month","note","key"]))
    keysBook          = ("book",
                        (["author","title","publisher","year"],
                         ["volume","series","edition","month","note","key","pages","address"]))--Editorissue
    keysBooklet       = ("booklet",
                        (["title"],
                         ["author","howpublished","address","month","year","note","key"]))
    keysConference    = ("conference",
                        (["author","title","booktitle","year"],
                         ["editor","pages","organization","publisher","address","month","note","key"]))
    keysInbook        = ("inbook",
                        (["author","title","chapter","pages","publisher","year"],
                         ["volume","series","address","edition","month","note","key","pages"]))--editorissue
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
    
