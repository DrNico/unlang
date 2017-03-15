

module UnSyntax
where

import UnLex                    (AlexPosn)

import Data.ByteString.Lazy.Char8
import Data.ByteString.Short    (ShortByteString)
import Data.Map                 (Map)
import qualified Data.Map as M
import Text.PrettyPrint

type Posn = AlexPosn


data Located a = Loc Posn a
type Name = ByteString

data Module
    = Module    (Located ByteString) [Decl]

data Decl
    = Decl      (Located Name) (Located Type) (Located Type)

data Type
    = Type
    | Var       (Located Name)
    | Arr       (Located Type) (Located Type)
    | Abs       (Located Name) (Located Type)
    | App       (Located Type) (Located Type)
    | All       (Located Name) (Located Type)

getLoc :: Located a -> Posn
getLoc (Loc pos _) = pos


class Pretty a where
    pPrint     :: a -> Doc
    pPrint = pPrintPrec 0

    pPrintPrec :: Int -> a -> Doc

    pShow      :: a -> String
    pShow = render . pPrint


instance Pretty Module where
    pPrintPrec _ (Module (Loc _ name) decls) =
        text "#module" <+> (text $ unpack name)
        $$ nest 4 (vcat $ fmap pPrint decls)
        $$ text "#end"

instance Pretty Decl where
    pPrintPrec _ (Decl (Loc _ name) (Loc _ typ) (Loc _ trm)) =
        (text $ unpack name) <+>
            (    text "::" <+> pPrint typ
            $$   text ":=" <+> pPrint trm <+> char ';'
            )
        $$ char '\n'

instance Pretty Type where
    pPrintPrec _ Type =
        text "Type"
    pPrintPrec _ (Var (Loc _ name)) =
        text $ unpack $ name
    pPrintPrec prec (Arr (Loc _ a) (Loc _ b)) =
        maybeParens (prec > 9) $
            pPrintPrec 10 a <+> text "->" <+> pPrintPrec 9 b
    pPrintPrec prec (Abs (Loc _ x) (Loc _ m)) =
        char '\\' <> (text $ unpack x) <> char '.' <+> pPrintPrec 0 m
    pPrintPrec prec (App (Loc _ m) (Loc _ n)) =
        maybeParens (prec > 10) $
            pPrintPrec 10 m <+> pPrintPrec 11 n
    pPrintPrec prec (All (Loc _ a) (Loc _ t)) =
        text "forall " <> (text $ unpack a) <> char '.' <+> pPrintPrec 0 t


maybeParens :: Bool -> Doc -> Doc
maybeParens False doc = doc
maybeParens True  doc = parens doc
