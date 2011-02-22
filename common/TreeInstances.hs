module Common.TreeInstances where
    
    import CCO.Tree            (ATerm (App,List,String), Tree (fromTree, toTree))
    import CCO.Tree.Parser
    import Common.BibTypes
    import Common.HtmlTypes
    import Control.Applicative

    instance Tree BibTex where
        fromTree (BibTex p l) = App "BibTex" [List (map fromTree p), List (map fromTree l)]
        toTree = parseTree [app "BibTex" (BibTex <$> arg <*> arg)]

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


    instance Tree Html where
        fromTree (Html head body) = App "html" [fromTree head, List $ map fromTree body]
        toTree = parseTree [app "html" (Html <$> arg <*> arg)]

    instance Tree Head where
        fromTree (Head title) = App "head" [fromTree title]
        toTree = parseTree [app "head" (Head <$> arg)]

    instance Tree BlockElem where
        fromTree (A attr ref)     = App "a"      [List $ map fromTree attr, fromTree ref]
        fromTree Hr               = App "hr"     []  
        fromTree (Table attr tr)  = App "table"  [List $ map fromTree attr, List $ map fromTree tr] 
        fromTree (P attr str)     = App "p"      [List $ map fromTree attr, fromTree str]
        toTree                    = parseTree  [ app "a"      (A   <$> arg <*> arg)
                                               , app "hr"     (pure Hr) 
                                               , app "table"  (Table <$> arg <*> arg)
                                               , app "p"      (P <$> arg <*> arg)
                                               ]   

    instance Tree Tr where
        fromTree (Tr attr td) = App "tr" [List $ map fromTree attr, List $ map fromTree td] 
        toTree = parseTree [app "tr" (Tr <$> arg <*> arg)]
