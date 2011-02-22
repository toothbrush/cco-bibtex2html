module Main where
    
    import CCO.Component hiding (parser)
    import CCO.Feedback
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree), parser)
    import Common.BibTypes
    import Common.HtmlTypes
    import Common.TreeInstances
    import Common.ATermUtils
    import Control.Arrow
    import Data.List (intersperse,nub,group,sort)
    
    main :: IO ()
    main = ioWrap pipeline
   
    pipeline :: Component String String
    pipeline =  parser        >>> 
                aTerm2BibTex  >>> 
                checkDups     >>> {-
                validator     >>>   
                sorter        >>> -}
                bibTex2HTML   >>>   
                html2Aterm    >>>
                aTerm2String
   

    checkDups :: Component BibTex BibTex
    checkDups = component (\inp@(BibTex entries) -> do trace_ "Checking for duplicate entry identifiers..."
                                                       let references = map reference entries 
                                                       if (length references /= (length$nub references)) then
                                                           do let grouped = map (\xs@(x:_) -> (x, length xs)) . group . sort $ references
                                                              let filtered = filter (\(_,c)-> c>1) grouped 
                                                              let dups = concatMap ((\a-> " > "++a++"\n").fst) filtered
                                                       	      warn_ ("The following duplicate keys were found:\n" ++ dups)
                                                         else 
                                                       	   trace_ "OK, no duplicates."
                                                       return inp)
  
    bibTex2HTML :: Component BibTex Html
    bibTex2HTML = component (\inp -> do trace_ "Converting BibTeX to HTML..."
                                        return $ foldBibTex bib2htmlAlg inp)

    bib2htmlAlg :: BibTexAlgebra Html ([BlockElem],Tr)
    bib2htmlAlg = (fBibTex,fEntry) where
        fBibTex entries       =  let tableEntries = (snd . unzip) entries
                                     arefs        = (concat . fst . unzip) entries
                                 in  Html (Head "Bibliography") $ 
                                     (separate arefs) ++ [Hr, Table [(Field "border" "0")] tableEntries]
        fEntry spec ref attr  =  (generateIndex ref, generateTableRows spec ref attr)

    separate :: [BlockElem] -> [BlockElem]
    separate = intersperse (P [] "|")

    generateIndex :: Reference -> [BlockElem]
    generateIndex ref = [ A [Field "href" ('#':ref)] ref ]

    generateTableRows :: EntryType -> Reference -> [Field] -> Tr
    generateTableRows spec ref attr = Tr [Field "valign" "top"
                                         ] 
                                         [ A [Field "name" ref] ref
                                         , P [] (flattenEntry spec attr)
                                         ]

    flattenEntry :: EntryType -> [Field] -> String
    flattenEntry spec = foldr ((++) . formatFields) ""


    formatFields :: Field -> String
    formatFields (Field field value) 
                        | field == "author"     = value++". "
                        | field == ""           = ""          -- ignore fields that don't belong to the entry
                        | field == "booktitle"  = "<em>"++value++"</em>, "
                        | field == "title"      = "<em>"++value++"</em>, "
                        | field == "pages"      = "pages "++value++". "
                        | field == "year"       = value++". "
                        | field == "editor"     = "In: "++value++", editors, "
                        | otherwise             = value++", "
  
