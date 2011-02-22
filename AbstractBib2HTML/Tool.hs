-- | this module implements the bib2html tool. It contains the complete pipeline for
-- this part of the suite, converting from a BibTex ATerm into an Html ATerm.
-- This is also where the main validation is done on the BibTeX database, to make sure that there
-- are no duplicates, etc. 
module AbstractBib2HTML.Tool where
    
    import CCO.Component hiding (parser)
    import CCO.Feedback
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree), parser)
    import Common.BibTypes
    import Common.HtmlTypes
    import Common.TreeInstances
    import Common.AllowedFields
    import Common.ATermUtils
    import Control.Arrow
    import Data.Maybe
    import Data.List
    
    -- | the default main function. Just wrap a pipeline in IO
    mainFunc :: IO ()
    mainFunc = ioWrap pipeline
   
    -- | The pipeline for bib2html. 
    pipeline :: Component String String
    pipeline =  parser        >>> 
                aTerm2BibTex  >>> 
                checkDups     >>> 
                checkRequired >>> 
                sorter        >>> 
                bibTex2HTML   >>>   
                html2Aterm    >>>
                aTerm2String
   
    -- | checks a number of things. Make sure all required fields are in all entries, and that there are no fields 
    -- which are unrecognised (such as misspelled fields). Also sorts the fields (author, title, the rest, finally year). Finally
    -- checks that all the other fields are at least optional, since we don't want disallowed fields in entries. 
    checkRequired :: Component BibTex BibTex
    checkRequired = component $ (\(BibTex pa es) -> do trace_ "Checking required and optional fields..."
                                                       checkedEntries <- mapM checkEntry es
                                                       let nonempties =  filter empty checkedEntries -- illegal fields are replaced with empty, so filter these out.
                                                       withSortedFields <- mapM sortFields nonempties -- sort the fields
                                                       checkedOptionals <- mapM checkOptionals withSortedFields -- check all fields are at least optional
                                                       return (BibTex pa checkedOptionals)
                                )

    -- | this function returns whether a given entry has 0 fields. We don't want those. 
    empty :: Entry -> Bool
    empty e = (not.null) (fields e)

    -- | Sort fields, and nub duplicates. Here we just need to call `sort`, since
    -- the Ord class is implemented on Fields.
    sortFields :: Entry -> Feedback Entry
    sortFields = (\e -> do  let entrytype = entryType e
                            let ref = reference e
                            let fs = fields e
                            trace_ ("Sorting fields for entry ["++ref++"]...")
                            if (length fs /= length (nub fs)) 
                            	then warn_ "WARNING: Duplicate field definitions found!"
                            	else return ()
                            return (Entry entrytype ref (sort (nub fs)))
                 )

    -- | Checks that all fields in an entry are at least optional. If they are not, issue a warning and empty them. These will later be pruned.
    checkOptionals :: Entry -> Feedback Entry
    checkOptionals = (\e -> do  let entrytype = entryType e
                                let ref = reference e
                                let fs = fields e
                                trace_ ("Checking entry ["++ref++"] for optional fields...")
                                let (req,opt) = fromJust (lookup entrytype allowedTable)
                                let filledIn = map getKey fs
                                let optFieldsExistence = map (\i -> (flip elem (opt++req) i, i)) filledIn
                                let missings = map snd $ filter (not.fst) optFieldsExistence
                                if null missings 
                                    then return e
                                    else do warn_ ("WARNING: nonexistent field(s) found and ignored: \n"++(concatMap (\i -> " > "++i++"\n") missings))
                                            return (Entry entrytype ref (fs\\(map (\str -> Field str "") missings)))
                 )

    -- | Check that all required fields are present, depending on the entry's type. If they aren't issue an error and stop. This is the only 
    -- condition on which the bib2html program fails. 
    checkEntry :: Entry -> Feedback Entry
    checkEntry = (\e -> do  let entrytype = entryType e
                            let ref = reference e
                            let fs = fields e
                            trace_ ("Checking entry ["++ref++"] for required fields...")
                            case lookup entrytype allowedTable of
                                Nothing         -> do warn_ ("WARNING: Entry type \"" ++ entrytype ++ "\" doesn't exist, entry ommitted!")
                                                      return (Entry "" "" [])
                                Just (req,opt)  -> do let filledIn = map getKey fs
                                                      let reqFieldsExistence = map (\i -> (flip elem filledIn i, i)) req
                                                      let missings = map snd $ filter (not.fst) reqFieldsExistence
                                                      if null missings 
                                                        then return e
                                                        else fail ("ERROR: required fields not found: \n"++(concatMap (\i -> " > "++i++"\n") missings))
                 )

    -- | Sort entries by author, year, title. Makes use of the usual lexical sort on strings. 
    sorter :: Component BibTex BibTex
    sorter = component $ (\(BibTex pa entries) -> do trace_ "Sorting bibliography..."
                                                     return (BibTex pa (
                                                           (sortBy (sortGen "author")
                                                               (sortBy (sortGen "year")
                                                                   (sortBy (sortGen "title") 
                                                                       entries
                                                                   ) 
                                                               )
                                                           )
                                                        )))

    -- | helper function which compares two entries and returns an ordering, based on the key requested.
    sortGen :: String -- ^ the key to sort on
            -> Entry  -- ^ entry 1
            -> Entry  -- ^ entry 2
            -> Ordering
    sortGen key e1 e2 = compare (maybegetValue v1) (maybegetValue v2)
            where v1 = lookupField key $ fields e1
                  v2 = lookupField key $ fields e2


    -- | This function eliminates entries with the same name. The first found entry with a certain name is retained. 
    -- A warning is also issued. 
    checkDups :: Component BibTex BibTex
    checkDups = component (\inp@(BibTex pa entries) -> do trace_ "Checking for duplicate entry identifiers..."
                                                          let references = map reference entries 
                                                          if (length references /= (length$nub references)) then
                                                              do let grouped = map (\xs@(x:_) -> (x, length xs)) . group . sort $ references
                                                                 let filtered = filter (\(_,c)-> c>1) grouped 
                                                                 let dups = concatMap ((\a-> " > "++a++"\n").fst) filtered
                                                                 warn_ ("WARNING: The following duplicate keys were found (and ignored):\n" ++ dups)
                                                            else 
                                                               return ()
                                                          return (BibTex pa (nub entries))
                                                          )
  
    -- | Convert a BibTex tree to Html by folding with the bib2htmlAlg algebra.
    bibTex2HTML :: Component BibTex Html
    bibTex2HTML = component (\inp -> do trace_ "Converting BibTeX to HTML..."
                                        return $ foldBibTex bib2htmlAlg inp)

    -- | this algebra converts a BibTex tree into an Html representation, including
    -- a list of hyperlinks at the top, maybe preamble blocks, and the table with the
    -- entries. 
    bib2htmlAlg :: BibTexAlgebra Html BlockElem ([BlockElem],Tr)
    bib2htmlAlg = (fBibTex, fPa, fEntry) where
        fBibTex pa entries    =  let tableEntries = (snd . unzip) entries
                                     arefs        = (concat . fst . unzip) entries
                                 in  Html (Head "Bibliography") $ 
                                     (separate arefs) ++ preamble pa ++ [Hr, Table [(Field "border" "0")] tableEntries]
        fEntry spec ref attr  =  (generateIndex ref, generateTableRows spec ref attr)
        fPa ss = P [Field "style" "border-style: solid;"] ss
        preamble pa = if null pa then []
                      else Hr : pa

    -- | used to place a pipe character between the list of hyperlinks 
    separate :: [BlockElem] -> [BlockElem]
    separate = intersperse (P [] "|")

    -- | turns a reference into a hyperlink 
    generateIndex :: Reference -> [BlockElem]
    generateIndex ref = [ A [Field "href" ('#':ref)] ref ]

    -- | turns an Entry (or more accurately, given a type, a name, and a list of attributes) into
    -- a table row
    generateTableRows :: EntryType -> Reference -> [Field] -> Tr
    generateTableRows spec ref attr = Tr [Field "valign" "top"
                                         ] 
                                         [ A [Field "name" ref] ref
                                         , P [] (flattenEntry spec attr)
                                         ]


    -- | given a type and a list of attributes, flattens an entry into a string, for placement
    -- in a table cell
    flattenEntry :: EntryType -> [Field] -> String
    flattenEntry spec = foldr ((++) . formatFields) ""

    -- | format a field into a string. Possibly do special formatting things, depending on 
    -- if it's a title, for example. 
    formatFields :: Field -> String
    formatFields (Field field value) 
                        | field == "author"     = value++". "
                        | field == ""           = ""          -- ignore fields that don't belong to the entry
                        | field == "booktitle"  = "<em>"++value++"</em>, "
                        | field == "title"      = "<em>"++value++"</em>, "
                        | field == "pages"      = "pages "++value++". "
                        | field == "year"       = value++". "
                        | field == "url"        = "<a href=\""++value++"\">"++value++"</a>, "
                        | field == "editor"     = "In: "++value++", editors, "
                        | otherwise             = value++", "
  
