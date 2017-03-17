
module UnPretty where

import Text.PrettyPrint

class Pretty a where
    pPrint     :: a -> Doc
    pPrint = pPrintPrec 0

    pPrintPrec :: Int -> a -> Doc

    pShow      :: a -> String
    pShow = render . pPrint


class PrettyIO a where
    pPrintIO        :: a -> IO Doc
    pPrintIO = pPrintPrecIO 0

    pPrintPrecIO    :: Int -> a -> IO Doc

    pShowIO         :: a -> IO String
    pShowIO x = do
        doc <- pPrintIO x
        return $ render doc

maybeParens :: Bool -> Doc -> Doc
maybeParens False doc = doc
maybeParens True  doc = parens doc
