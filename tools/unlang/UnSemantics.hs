
module UnSemantics where

import UnPretty

import Data.IORef
import Text.PrettyPrint

-----
-- Type
-----

data Type
  = Type    LvlRef
  | Var     SlotRef
  | Arr     Type Type
  | Abs     SlotRef Type
  | App     Type Type
  | All     SlotRef Type

-----
-- Variables
-----

type SlotRef = IORef Slot

data Slot
  = Free    Type
  | Bound   Type

mkType :: IO Type
mkType = do
    lvl <- mkLvl
    return $ Type lvl

mkTypeVar :: IO Type
mkTypeVar = do
    typ <- mkType
    mkVar typ

mkTermVar :: IO Type
mkTermVar = do
    typ <- mkTypeVar
    mkVar typ

mkVar :: Type -> IO Type
mkVar typ = do
    slot <- newIORef $ Free typ
    return $ Var slot

-----
-- Type Levels
-----

type LvlRef = IORef Level

data Level
  = LvlFree
  | LvlBound    LvlRef
  | LvlPred     LvlRef

mkLvl :: IO LvlRef
mkLvl =
    newIORef LvlFree


-----
-- Operations on Types
-----

kind :: Type -> IO Type
-- kind (Type lref) = do
--     ref <- newIORef $ LvlSucc lref
--     return $ Type ref
kind (Var sref) = do
    slot <- readIORef sref
    case slot of
        Free typ  -> return typ
        Bound typ -> kind typ
kind (Arr a b) =
    kind b
kind (Abs xref m) = do
    a <- kind (Var xref)
    b <- kind m
    return $ Arr a b
kind (App m n) =
    kind m
kind (All aref t) =
    kind t


instance PrettyIO Type where
    pPrintPrecIO _ (Type _) =
        return $ text "Type"
--    pPrintPrec _ (Var slot) = do
    pPrintPrecIO prec (Arr a b) = do
        doca <- pPrintPrecIO 10 a
        docb <- pPrintPrecIO 9  b
        return $ maybeParens (prec > 9) $
            doca <+> text "->" <+> docb
    -- pPrintPrec prec (Abs (Loc _ x) (Loc _ m)) =
    --     char '\\' <> (text $ unpack x) <> char '.' <+> pPrintPrec 0 m
    -- pPrintPrec prec (App (Loc _ m) (Loc _ n)) =
    --     maybeParens (prec > 10) $
    --         pPrintPrec 10 m <+> pPrintPrec 11 n
    -- pPrintPrec prec (All (Loc _ a) (Loc _ t)) =
    --     text "forall " <> (text $ unpack a) <> char '.' <+> pPrintPrec 0 t
