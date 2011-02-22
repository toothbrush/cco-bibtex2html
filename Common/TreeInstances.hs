-- | This module contains instances of Tree for various data structures. These are used 
-- for converting between ATerm and the given data structure. These functions aren't very 
-- complicated, they just allow 'flattening' a datastructure into a portable format, and 
-- converting it back again.
--
module Common.TreeInstances where
    
    import CCO.Tree            (ATerm (App,List,String), Tree (fromTree, toTree))
    import CCO.Tree.Parser
    import CCO.Feedback
    import Common.BibTypes
    import Common.HtmlTypes
    import Control.Applicative

    instance Tree BibTex where
        fromTree = bibfromTree
        toTree   = bibtoTree

    -- | Converts from BibTex to ATerm
    bibfromTree :: BibTex -> ATerm
    bibfromTree (BibTex p l) = App "BibTex" [List (map fromTree p), List (map fromTree l)]

    -- | Converts an ATerm back into a BibTex tree
    bibtoTree :: ATerm -> Feedback BibTex
    bibtoTree = parseTree [app "BibTex" (BibTex <$> arg <*> arg)]

    instance Tree Entry where
        fromTree = entryfromTree
        toTree   = entrytoTree

    -- | Converts from bibtex Entry to ATerm
    entryfromTree :: Entry -> ATerm
    entryfromTree e = App "Entry" [ fromTree $ entryType e         -- type
                             , fromTree $ reference e         -- ref
                             , List $ map fromTree (fields e) -- fields 
                             ] 

    -- | Converts an ATerm back into a BibTex entry
    entrytoTree :: ATerm -> Feedback Entry
    entrytoTree = parseTree [app "Entry" (Entry   <$> arg <*> arg <*> arg)]


    instance Tree Field where
        fromTree = fieldfromTree
        toTree   = fieldtoTree

    -- | Converts from Field (key/value pair) to ATerm
    fieldfromTree :: Field -> ATerm
    fieldfromTree (Field k v) = App "Field" [ String k -- key 
                                       , String v -- value
                                       ]

    -- | Converts an ATerm back into a key/value pair
    fieldtoTree :: ATerm -> Feedback Field
    fieldtoTree = parseTree [app "Field" (Field <$> arg <*> arg)]


    instance Tree Html where
        fromTree = htmlfromTree
        toTree   = htmltoTree

    -- | Converts from HTML document to ATerm
    htmlfromTree :: Html -> ATerm
    htmlfromTree (Html head body) = App "html" [fromTree head, List $ map fromTree body]

    -- | Converts an ATerm back into an HTML document
    htmltoTree :: ATerm -> Feedback Html
    htmltoTree = parseTree [app "html" (Html <$> arg <*> arg)]

    instance Tree Head where
        fromTree = headfromTree
        toTree   = headtoTree

    -- | Converts from HTML head element to ATerm
    headfromTree :: Head -> ATerm
    headfromTree (Head title) = App "head" [fromTree title]

    -- | Converts an ATerm back into an html head element
    headtoTree :: ATerm -> Feedback Head
    headtoTree = parseTree [app "head" (Head <$> arg)]

    instance Tree BlockElem where
        fromTree = blockitemfromTree
        toTree   = blockitemtoTree

    -- | Converts from HTML entity to ATerm
    blockitemfromTree :: BlockElem -> ATerm
    blockitemfromTree (A attr ref)     = App "a"      [List $ map fromTree attr, fromTree ref]
    blockitemfromTree Hr               = App "hr"     []  
    blockitemfromTree (Table attr tr)  = App "table"  [List $ map fromTree attr, List $ map fromTree tr] 
    blockitemfromTree (P attr str)     = App "p"      [List $ map fromTree attr, fromTree str]

    -- | Converts an ATerm back into an html element
    blockitemtoTree :: ATerm -> Feedback BlockElem
    blockitemtoTree                    = parseTree  [ app "a"      (A   <$> arg <*> arg)
                                           , app "hr"     (pure Hr) 
                                           , app "table"  (Table <$> arg <*> arg)
                                           , app "p"      (P <$> arg <*> arg)
                                           ]   

    instance Tree Tr where
        fromTree = trfromTree
        toTree   = trtoTree

    -- | Converts from HTML horizontal rule to ATerm
    trfromTree :: Tr -> ATerm
    trfromTree (Tr attr td) = App "tr" [List $ map fromTree attr, List $ map fromTree td] 

    -- | Converts an ATerm back into a horizontal rule (html element)
    trtoTree :: ATerm -> Feedback Tr
    trtoTree = parseTree [app "tr" (Tr <$> arg <*> arg)]
