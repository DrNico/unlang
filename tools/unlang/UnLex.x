{

module UnLex where

import Data.ByteString.Lazy            ( ByteString )
-- import Data.ByteString as BS        ( ByteString, uncons, drop, splitAt )
import Data.ByteString.Char8 as BS  ( unpack )
-- import GHC.Exts                     ( Int(..), Int#, (+#), (/=#) )
-- import Data.Word                    ( Word8 )
}

%wrapper "posn-bytestring"

$alpha      = [A-Za-z]
$digit      = [0-9]
@ident      = $alpha [$alpha $digit _]*
-- @modname    = $alpha [$alpha $digit]*

:-

    $white+                         ;
    "#" [\ \t] [^\n]*               ;
    "#" $                           ;
--  \n                              { \p s -> (p,TokNewLine) }
    "#module" / $white              { \p s -> (p,TokenTerminal 1) }
    "("                             { \p s -> (p,TokenTerminal 2) }
    ")"                             { \p s -> (p,TokenTerminal 3) }
    ","                             { \p s -> (p,TokenTerminal 4) }
    "->"                            { \p s -> (p,TokenTerminal 5) }
    "."                             { \p s -> (p,TokenTerminal 6) }
    ":"                             { \p s -> (p,TokenTerminal 7) }
    ":-"                            { \p s -> (p,TokenTerminal 8) }
    "::"                            { \p s -> (p,TokenTerminal 9) }
    ":="                            { \p s -> (p,TokenTerminal 10) }
    ";"                             { \p s -> (p,TokenTerminal 11) }
    "Type"                          { \p s -> (p,TokenTerminal 12) }
    \\                              { \p s -> (p,TokenTerminal 13) }
    "forall"                        { \p s -> (p,TokenTerminal 14) }
    "#end" / $white                 { \p s -> (p,TokenTerminal 15) }
    @ident                          { \p s -> (p,TokenIdent s) }

{

data Token
  = TokenIdent ByteString
  | TokenTerminal Int
  | TokenEOF
  deriving (Eq)

{-
type AlexInput = (Posn,ByteString)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos,input) = case BS.uncons input of
    Just (w,rest) ->

alexInputPrevChar :: ByteString -> Char
alexInputPrevChar = undefined

data Posn = Posn !Int# !Int#

adv :: ByteString -> Posn -> Posn
adv bs pos = case BS.uncons bs of
    Just (w,rest) -> adv rest (pos `mappend` mkPos w)
    Nothing       -> pos
    where
        mkPos w
            | w .&. 0x80 /= 0 = mempty
            | w == 0x0A       = Posn 1# 0#
            | otherwise       = Posn 0# 1#

instance Monoid Posn where
    mempty = Posn 0# 0#
    mappend (Posn r1 c1) (Posn r2 c2) =
        if (I# r2) /= 0
        then Posn (r1 +# r2) 0#
        else Posn r1 (c1 +# c2)

instance Show Posn where
    showsPrec _ (Posn c r) =
        showChar '(' . shows (I# c) . showChar ',' . shows (I# r) . showChar ')'
-}

instance Show Token where
    show (TokenIdent s)    = show s -- BS.unpack s        -- TODO add Unicode support
    show (TokenTerminal 1) = "#module"
    show (TokenTerminal 2) = "("
    show (TokenTerminal 3) = ")"
    show (TokenTerminal 4) = ","
    show (TokenTerminal 5) = "->"
    show (TokenTerminal 6) = "."
    show (TokenTerminal 7) = ":"
    show (TokenTerminal 8) = ":-"
    show (TokenTerminal 9) = "::"
    show (TokenTerminal 10) = ":="
    show (TokenTerminal 11) = ";"
    show (TokenTerminal 12) = "Type"
    show (TokenTerminal 13) = "\\"
    show (TokenTerminal 14) = "where"
    show (TokenTerminal 15) = "#end"
    show (TokenEOF)         = "<EOF>"

}
