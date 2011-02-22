module Common.HtmlTypes where

    import Common.BibTypes

    data Html = Html Head Body
                deriving Show
  
    data Head = Head Title
                deriving Show
  
    type Body = [BlockElem]
  
    data BlockElem = A [Field] Reference
                   | Hr
                   | Table [Field] [Tr]
                   | P [Field] String
                   deriving Show
  
    data Tr   = Tr [Field] [Td]
                deriving Show
  
    type Td        = BlockElem
    type Title     = String

    type HtmlAlgebra html head body block tr = (
                                                    head -> body -> html,
                                                    (
                                                        Title   -> head,
                                                        [block] -> body
                                                    ),
                                                    (
                                                        [Field] -> Reference -> block,
                                                                                block,
                                                        [Field] -> [tr]      -> block,
                                                        [Field] -> String    -> block
                                                    ),
                                                    [Field] -> [block] -> tr
                                                )
    foldHtml :: HtmlAlgebra html head body block tr -> Html -> html
    foldHtml (
                fhtml,
                (fhead, fbody), 
                (fa, fhr, ftable, fp),
                ftr
             ) = foldHtml' where
                 foldHtml' (Html h b)     = fhtml (foldHead h) (foldBody b)
                 foldHead  (Head title)   = fhead title
                 foldBody                 = fbody . map foldBlock
                 foldBlock (A fs ref)     = fa fs ref
                 foldBlock (Hr)           = fhr
                 foldBlock (Table fs trs) = ftable fs (map foldTr trs)
                 foldBlock (P fs txt)     = fp fs txt
                 foldTr    (Tr fs tds)    = ftr fs (map foldBlock tds)




















