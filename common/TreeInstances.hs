module Common.TreeInstances where
    
    import CCO.Tree            (ATerm (App,List,String), Tree (fromTree, toTree))
    import Common.Types

    instance Tree BibTex where
        fromTree (BibTex l) = App "BibTex" [List (map fromTree l)]

    instance Tree Entry where
        fromTree e = App "Entry" [ fromTree $ entryType e         -- type
                                 , fromTree $ reference e         -- ref
                                 , List $ map fromTree (fields e) -- fields 
                                 ] 


    instance Tree Field where
        fromTree (Field k v) = App "Field" [ String k -- key 
                                           , String v -- value
                                           ]

