-- | This module contains our representation of an HTML document. Our 
-- simplistic version of HTML only knows anchors, horizontal rules, tables and paragraphs, 
-- but this is enough to display a BibTeX database. 
module Common.HtmlTypes where

    import Common.BibTypes

    -- | an HTML document has a head and a body
    data Html = Html Head Body
                deriving Show
  
    -- | the head only contains the title.
    data Head = Head Title
                deriving Show
  
    -- | the body of an HTML document is a list of block elements (table, anchor, etc.)
    type Body = [BlockElem]
  
    -- | a block element can be (as stated) anchor, rule, table or a paragraph (a paragraph
    -- is also used to represent a plain string. In this case it's attribute list is empty, 
    -- and when rendering, the p-tag is ommitted.
    data BlockElem = A [Field] Reference
                   | Hr
                   | Table [Field] [Tr]
                   | P [Field] String
                   deriving Show
  
    -- | a table row. It has attributes and a list of cells
    data Tr   = Tr [Field] [Td]
                deriving Show
  
    -- | a table cell is a block element
    type Td        = BlockElem
    -- | the title of a document is simply a string. 
    type Title     = String

    -- | And once again we define an algebra for folding over an HTML document.
    -- This will prove useful when we want to pretty-print the html (or render
    -- it in any form), since the above type is only an abstract representation of 
    -- an HTML document.
    --
    -- Since Html is tree-like, it seems logical to use a fold to convert it to some other
    -- (tree-like) format, in our case, this is Doc, the CCO pretty-print data structure. The actual
    -- fold for this is defined in PrettyPrintHTML.Tool. 
    type HtmlAlgebra html head body block tr = (
                                                    head -> body -> html,  --  how to replace the Html constructor
                                                    (
                                                        Title   -> head,   --  how to replace the Head constructor
                                                        [block] -> body    --  likewise for the Body constructor
                                                    ),
                                                    (
                                                        [Field] -> Reference -> block, --  function for anchors
                                                                                block, --  what to do with a horizontal rule
                                                        [Field] -> [tr]      -> block, --  table
                                                        [Field] -> String    -> block  --  paragraph
                                                    ),
                                                    [Field] -> [block] -> tr --  table row
                                                )

    -- | The function which folds over an HTML tree, given an algebra as defined above.
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




















