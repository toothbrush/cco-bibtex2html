module Main where
    
    import CCO.Component hiding (parser)
    import CCO.Feedback
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree), parser)
    import Common.BibTypes
    import Common.HtmlTypes
    import Common.TreeInstances
    import Common.ATermUtils
    import Control.Arrow
    
    main :: IO ()
    main = ioWrap pipeline
   
    pipeline :: Component String String
    pipeline =  parser        >>> 
                aTerm2BibTex  >>> {-
                validator     >>>   
                sorter        >>> -}
                bibTex2HTML   >>>   
                html2Aterm    >>> -- easy
                aTerm2String      --done
   

    aTerm2BibTex :: Component ATerm BibTex
    aTerm2BibTex = component (\inp -> do trace_ "Converting ATerm to BibTeX..."
                                         toTree inp)
 
    html2Aterm :: Component Html ATerm
    html2Aterm = component (\inp -> do trace_ "Converting HTML to ATerm..."
                                       return $ fromTree inp)
  
    bibTex2HTML :: Component BibTex Html
    bibTex2HTML = component (\inp -> do trace_ "Converting BibTeX to HTML..."
                                        return $ foldBibTex bib2htmlAlg inp)

    bib2htmlAlg :: BibTexAlgebra Html ([BlockElem],Tr)
    bib2htmlAlg = (fBibTex,fEntry) where
        fBibTex entries       =  let tableEntries = (snd . unzip) entries
                                     arefs        = (concat . fst . unzip) entries
                                 in  Html (Head "Bibliography") $ arefs ++  
                                     [Hr, Table [(Field "border" "0")] tableEntries]
        fEntry spec ref attr  =  (buildbegin ref, buildend spec ref attr)


    buildbegin :: Reference -> [BlockElem]
    buildbegin ref =  [A [(Field "href" ref)] ref
                      ,P []                   "|"
                      ] 

    buildend :: EntryType -> Reference -> [Field] -> Tr
    buildend spec ref attr = Tr [(Field "valign" "top")] [A [(Field "name" ref)] ref, P [] (buildPara spec attr)]

    buildPara :: EntryType -> [Field] -> String
    buildPara spec = foldr ((++) . cPAux) ""


    cPAux :: Field -> String
    cPAux (Field field value) 
                        | field == "author"     = value++". "
                        | field == ""           = ""          -- ignore fields that don't belong to the entry
                        | field == "booktitle"  = "<em>"++value++"</em>, "
                        | field == "title"      = "<em>"++value++"</em>, "
                        | field == "pages"      = "pages "++value++". "
                        | field == "year"       = value++"."
                        | field == "editor"     = "In: "++value++", editors, "
                        | otherwise             = value++", "
  
