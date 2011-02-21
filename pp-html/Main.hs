module Main where
    
    import Common.HtmlTypes
    import Common.TreeInstances
    import CCO.Component hiding (parser)
    import CCO.Feedback
    import CCO.Printing
    import Control.Arrow
    import CCO.Tree            (ATerm (App,List), Tree (fromTree, toTree), parser)

    main :: IO ()
    main =  ioWrap pipeline

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
                            printA     fs ref   =  text "unimplemented"
                            printHr             =  text "unimplemented"
                            printTable fs trs   =  text "unimplemented"
                            printP     fs txt   =  text "unimplemented"
                            printTr    fs bs    =  text "unimplemented"
