{-# LANGUAGE OverloadedStrings #-}

module UnEnv where

import UnSemantics                  ( Type )
import qualified UnSemantics as Sem
import UnSyntax                     ( Name, Posn )

import Control.Monad.IO.Class       ( liftIO )
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.ByteString.Short        ( ShortByteString )
import Data.Map as M
import Text.PrettyPrint             ( Doc, render )


type UnEnv a = ExceptT Doc (StateT Bindings IO) a

runEnv :: UnEnv () -> IO ()
runEnv m = do
    res <- evalStateT (runExceptT m) (M.singleton "" M.empty)
    case res of
        Right () ->
            return ()
        Left err ->
            putStrLn $ render err

type Bindings = Map Path (Map Name (Type,Type))


type Path = ShortByteString


tcError :: Posn -> Doc -> UnEnv a
tcError pos doc =
    throwE doc


mkType :: UnEnv Type
mkType = liftIO Sem.mkType

mkTypeVar :: UnEnv Type
mkTypeVar = liftIO Sem.mkTypeVar

mkTermVar :: UnEnv Type
mkTermVar = liftIO Sem.mkTermVar

mkVar :: Type -> UnEnv Type
mkVar = liftIO . Sem.mkVar
