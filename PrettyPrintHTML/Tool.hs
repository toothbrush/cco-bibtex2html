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

    pipeline :: Component String String
    pipeline = parser >>>
               aTerm2Html >>>
               html2String

    aTerm2Html :: Component ATerm Html
    aTerm2Html = component (\inp -> do trace_ "Converting ATerm to Html..."
                                       toTree inp)

    html2String :: Component Html String
    html2String = component (\inp -> do trace_ "Writing HTML file..."
                                        return (render_ 80 (foldHtml html2ppAlgebra inp)))

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

    tagWithFields :: [Field] -> String -> Doc
    tagWithFields fs tag = text ( "<" ++ tag)
                        >|< (foldr (>|<) empty (map printField fs))
                        >|< text ">"

    printField :: Field -> Doc
    printField (Field key val) = text $ " " ++ key ++ "=\"" ++ val ++ "\""
