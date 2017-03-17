{-# LANGUAGE OverloadedStrings #-}

module UnCheck where

import UnEnv
import UnPretty
import UnSyntax                     ( Located(..), Name )
import qualified UnSyntax as Syn
import qualified UnSemantics as Sem

import Control.Monad.IO.Class       ( MonadIO(..) )
import Control.Monad.Trans.Class    ( lift )
import Control.Monad.Trans.State    ( StateT, modify )
import Data.ByteString.Lazy.Char8   ( unpack )
import Data.ByteString.Short        ( ShortByteString )
import Data.IORef
import Data.Map as M
import Text.PrettyPrint


buildModule :: Syn.Module -> UnEnv ()
buildModule (Syn.Module _ decls) =
    mapM_ buildDecl decls

buildDecl :: Syn.Decl -> UnEnv ()
buildDecl (Syn.Decl (Loc _ name) typ trm) = do
    knd <- mkType
    typ' <- buildW typ knd
    trm' <- buildW trm typ'
    lift $ modify $ M.adjust (M.insert name (trm',typ')) ""

{- Build a semantic term from a syntactic term with specified target type.
   This routine essentially implements algorithm W.
-}
buildW :: Located Syn.Type -> Sem.Type -> UnEnv Sem.Type
buildW = go M.empty
    -- first argument stores 'lambda' and 'forall' binders
    where
    go :: Map Name Sem.Type -> Located Syn.Type -> Sem.Type -> UnEnv Sem.Type
    -- Type : <something>
    go binds (Loc loc Syn.Type) typ = case typ of
        -- Type_0 : Type_1
        Sem.Type lvl1 -> do
            lvl0 <- liftIO $ newIORef $ Sem.LvlPred lvl1
            return $ Sem.Type lvl0
        -- Type_0 : a
        Sem.Var slot1 -> do
            -- TODO yank variable
            var1 <- liftIO $ readIORef slot1
            case var1 of
                -- a : Type_2
                Sem.Free (Sem.Type lvl2) -> do
                    lvl1 <- liftIO $ newIORef $ Sem.LvlPred lvl2
                    -- bind 'a' to Type_1
                    liftIO $ writeIORef slot1 (Sem.Bound $ Sem.Type lvl1)
                    lvl0 <- liftIO $ newIORef $ Sem.LvlPred lvl1
                    return $ Sem.Type lvl0
                -- a = Type_1
                Sem.Bound (Sem.Type lvl1) -> do
                    lvl0 <- liftIO $ newIORef $ Sem.LvlPred lvl1
                    return $ Sem.Type lvl0
        -- Type : <invalid>
        other        -> do
            doco <- liftIO $ pPrintIO other
            tcError loc $ text "Type mismatch:"
                $$ nest 4 (text "required type: Type")
                $$ nest 4 (text "supplied type: " <> doco)
    -- lambda x. m : b
--    go binds (Loc _ (Syn.Abs (Loc _ x) m)) typ = do

    -- forall a. m : Type_1
    go binds (Loc _ (Syn.All (Loc _ syna) m)) typ = do
        -- TODO check:  typ = Type
        sema <- mkTermVar
        go (M.insert syna sema binds) m typ
