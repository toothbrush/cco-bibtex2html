-- | This module contains the pp-html tool's code. It is a rather simple tool: it 
-- reads an Html ATerm, folds over the Html to convert it into Doc (the data structure provided
-- by the CCO library for pretty-printing), and finally makes use of CCO's rendering function
-- to output the Html tree as a string. 
module PrettyPrintHTML.Tool where
    
    import Common.HtmlTypes
    import Common.TreeInstances
    import Common.BibTypes
    import CCO.Component hiding (parser)
    import CCO.Feedback
    import CCO.Printing
    import Control.Arrow
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree), parser)

    mainFunc :: IO ()
    mainFunc =  ioWrap pipeline

    -- | the pipeline for pp-html. Read input, fold over resulting Html tree, and 
    -- output with pretty-print built-in functions. 
    pipeline :: Component String String
    pipeline = parser >>>
               aTerm2Html >>>
               html2String

    -- | read an ATerm and return Html (abstract tree)
    aTerm2Html :: Component ATerm Html
    aTerm2Html = component (\inp -> do trace_ "Converting ATerm to Html..."
                                       toTree inp)

    -- | pretty print the Html tree. Do this using html2ppAlgebra, an algebra to fold 
    -- over the Html with. 
    html2String :: Component Html String
    html2String = component (\inp -> do trace_ "Writing HTML file..."
                                        return (render_ 80 (foldHtml html2ppAlgebra inp)))

    -- | the meat of this module. Defines an algebra that when used to fold over an Html 
    -- structure, yields a Doc, which can be pretty-printed. The way this is done, is that 
    -- for each possibly type found in Html, we define how to pretty-print it.
    html2ppAlgebra :: HtmlAlgebra Doc Doc Doc Doc Doc
    html2ppAlgebra = (printhtml, 
                      (printhead, printbody),
                      (printA, printHr, printTable, printP),
                      printTr
                      ) where 
                            printhtml head body =  text "<html>"
                                               >-< indent 2 head
                                               >-< indent 2 body
                                               >-< text "</html>"
                            printhead  title    =  text $ "<head><title>"++title++"</title></head>"
                            printbody  bs       =  text "<body>"
                                               >-< indent 2 (foldr (>-<) empty bs)
                                               >-< text "</body>"
                            printA     fs ref   =  tagWithFields fs "a"
                                               >|< text ref
                                               >|< text "</a>"
                            printHr             =  text "<hr>"
                            printTable fs trs   =  tagWithFields fs "table"
                                               >-< indent 2 (foldr (>-<) empty trs)
                                               >-< text "</table>"
                            printP     fs txt   =  if null fs then 
                                                    text txt
                                                   else tagWithFields fs "p" 
                                                    >-< indent 2 (text txt)
                                                    >-< text "</p>"
                            printTr    fs tds   =  tagWithFields fs "tr"
                                               >-< indent 2 (foldr (>-<) empty (map printTd tds))
                                               >-< text "</tr>"
                            printTd  blockelem  =  text "<td>"
                                               >-< indent 2 blockelem
                                               >-< text "</td>"

    -- | helper function to create an html tag given the element name and a list of attributes.
    tagWithFields :: [Field] -- ^ attribute list, pretty-printed with printField
                  -> String  -- ^ the tag name, A for example
                  -> Doc     -- ^ output pretty-print CCO format
    tagWithFields fs tag = text ( "<" ++ tag)
                        >|< (foldr (>|<) empty (map printField fs))
                        >|< text ">"

    -- | used to print an attribute of an Html element. Simple, key="value". 
    printField :: Field -> Doc
    printField (Field key val) = text $ " " ++ key ++ "=\"" ++ val ++ "\""
