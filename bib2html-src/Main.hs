module Main where
    
    import CCO.Component hiding (parser)
    import CCO.Feedback
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree), parser)
    import Common.BibTypes
    import Common.HtmlTypes
    import Common.TreeInstances
    import Common.AllowedFields
    import Common.ATermUtils
    import Control.Arrow
    import Data.List (intersperse,nub,group,sortBy,sort)
    
    main :: IO ()
    main = ioWrap pipeline
   
    pipeline :: Component String String
    pipeline =  parser        >>> 
                aTerm2BibTex  >>> 
                checkDups     >>> 
                checkRequired >>> 
                sorter        >>> 
                bibTex2HTML   >>>   
                html2Aterm    >>>
                aTerm2String
   
    checkRequired :: Component BibTex BibTex
    checkRequired = component $ (\(BibTex es) -> do trace_ "Checking required and optional fields..."
                                                    checkedEntries <- mapM checkEntry es
                                                    let nonempties =  filter empty checkedEntries
                                                    return (BibTex nonempties)
                                )

    empty :: Entry -> Bool
    empty e = (not.null) (fields e)

    checkEntry :: Entry -> Feedback Entry
    checkEntry = (\e -> do  let entrytype = entryType e
                            let ref = reference e
                            let fs = fields e
                            trace_ ("Checking entry ["++ref++"]...")
                            case lookup entrytype allowedTable of
                                Nothing         -> do warn_ ("Entry type \"" ++ entrytype ++ "\" doesn't exist, entry ommitted!")
                                                      return (Entry "" "" [])
                                Just (req,opt)  -> do let filledIn = map getKey fs
                                                      let reqFieldsExistence = map (\i -> (flip elem filledIn i, i)) req
                                                      let missings = map snd $ filter (not.fst) reqFieldsExistence
                                                      if null missings 
                                                      	  then return e
                                                          else fail ("ERROR: required fields not found: \n"++(concatMap (\i -> " > "++i++"\n") missings))
                 )

    sorter :: Component BibTex BibTex
    sorter = component $ (\(BibTex entries) -> do trace_ "Sorting bibliography..."
                                                  return (BibTex (
                                                        (sortBy (sortGen "author")
                                                            (sortBy (sortGen "year")
                                                                (sortBy (sortGen "title") 
                                                                    entries
                                                                ) 
                                                            )
                                                        )
                                                     )))

    sortGen :: String -> Entry -> Entry -> Ordering
    sortGen key e1 e2 = compare v1 v2
            where v1 = lookupField key $ fields e1
                  v2 = lookupField key $ fields e2


    checkDups :: Component BibTex BibTex
    checkDups = component (\inp@(BibTex entries) -> do trace_ "Checking for duplicate entry identifiers..."
                                                       let references = map reference entries 
                                                       if (length references /= (length$nub references)) then
                                                           do let grouped = map (\xs@(x:_) -> (x, length xs)) . group . sort $ references
                                                              let filtered = filter (\(_,c)-> c>1) grouped 
                                                              let dups = concatMap ((\a-> " > "++a++"\n").fst) filtered
                                                       	      warn_ ("WARNING: The following duplicate keys were found (and ignored):\n" ++ dups)
                                                         else 
                                                       	    trace_ "OK, no duplicates."
                                                       return $ BibTex (nub entries))
  
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
  
