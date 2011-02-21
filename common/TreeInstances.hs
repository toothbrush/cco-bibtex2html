module Common.TreeInstances where
    
    import CCO.Tree            (ATerm (App,List,String), Tree (fromTree, toTree))
    import CCO.Tree.Parser
    import Common.BibTypes
    import Control.Applicative

    instance Tree BibTex where
        fromTree (BibTex l) = App "BibTex" [List (map fromTree l)]
        toTree = parseTree [app "BibTex" (BibTex <$> arg)]

    instance Tree Entry where
        fromTree e = App "Entry" [ fromTree $ entryType e         -- type
                                 , fromTree $ reference e         -- ref
                                 , List $ map fromTree (fields e) -- fields 
                                 ] 
        toTree = parseTree [app "Entry" (Entry   <$> arg <*> arg <*> arg)]


    instance Tree Field where
        fromTree (Field k v) = App "Field" [ String k -- key 
                                           , String v -- value
                                           ]
        toTree = parseTree [app "Field" (Field <$> arg <*> arg)]

