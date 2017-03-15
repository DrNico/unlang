
{
module UnParse (
    parseModule, Posn, lexer1
) where

import UnLex
import UnSyntax

import Data.ByteString.Lazy         ( ByteString )
import Text.Show


}

%name       parseModule mod
%tokentype  { (Posn,Token) }
%monad      { P } { thenP } { returnP }
%error      { parseError }
%lexer      { lexer2 } { (_,TokenEOF) }

%token
    ident       { (_,TokenIdent _) }
    '#module'   { (_,TokenTerminal 1) }
    '('         { (_,TokenTerminal 2) }
    ')'         { (_,TokenTerminal 3) }
--    ','         { (_,TokenTerminal 4) }
    '->'        { (_,TokenTerminal 5) }
    '.'         { (_,TokenTerminal 6) }
--    ':'         { (_,TokenTerminal 7) }
--    ':-'        { (_,TokenTerminal 8) }
    '::'        { (_,TokenTerminal 9) }
    ':='        { (_,TokenTerminal 10) }
    ';'         { (_,TokenTerminal 11) }
    'Type'      { (_,TokenTerminal 12) }
    '\\'        { (_,TokenTerminal 13) }
    'forall'    { (_,TokenTerminal 14) }
    '#end'      { (_,TokenTerminal 15) }

%%

mod         :: { Module }
            : '#module' var decls '#end'          { Module $2 (reverse $3) }

decls       :: { [Decl] }
            : decls decl                { $2 : $1 }
            | {- empty -}               { [] }

decl        :: { Decl }
            : ident '::' type ':=' type ';'
            { Decl (Loc (fst $1) (getName $1)) $3 $5 }

type        :: { Located Type }
            : type2 '->' type           { Loc (getLoc $1) (Arr $1 $3) }
            | '\\' var type4            { Loc (fst $1) (Abs $2 $3) }
            | 'forall' var type5        { Loc (fst $1) (All $2 $3) }
            | type2                     { $1 }

type2       :: { Located Type }
            : type2 type3               { Loc (getLoc $1) (App $1 $2) }
            | type3                     { $1 }

type3       :: { Located Type }
            : 'Type'                    { Loc (fst $1) Type }
            | var                       { Loc (getLoc $1) (Var $1) }
            | '(' type ')'              { case $2 of Loc _ t -> Loc (fst $1) t }

type4       :: { Located Type }
            : var type4                 { Loc (getLoc $1) (Abs $1 $2) }
            | '.' type                  { $2 }

type5       :: { Located Type }
            : var type5                 { Loc (getLoc $1) (All $1 $2) }
            | '.' type                  { $2 }

var         :: { Located Name }
            : ident                     { Loc (fst $1) (getName $1) }

{

-- type Pos = AlexPosn

type P a = [(Posn,Token)] -> Either String a

returnP :: a -> P a
returnP x _ =
    return x

thenP :: P a -> (a -> P b) -> P b
thenP m f input =
    m input >>= \x -> f x input

parseError :: (Posn,Token) -> P a
parseError (pos,tok) input =
    fail $ show pos ++ ": unexpected token " ++ show tok

lexer1 :: ByteString -> [(Posn,Token)]
lexer1 = alexScanTokens

lexer2 :: ((Posn,Token) -> P a) -> P a
lexer2 cont [] = cont (alexStartPos,TokenEOF) []
lexer2 cont (tok:toks) = cont tok toks

getName :: (Posn,Token) -> ByteString
getName (_,TokenIdent s) = s

{-
lexer cont pos input =
    case alexScan input 0 of
        AlexEOF             -> cont TokenEOF pos input
        AlexError rest      -> fail $ show pos ++ ": lexing error"
        AlexSkip rest len   ->
            let skip = BS.take len input
                pos' = adv skip pos
            in pos' `seq` lexer cont pos' rest
        AlexToken rest len action ->
            -- TODO convert tok to shared ShortByteString
            let tok  = BS.take len input
                pos' = adv tok pos
            in pos' `seq` cont (action tok) pos' rest
-}


}
