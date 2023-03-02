{-# LANGUAGE InstanceSigs #-}

module Text.SimpleJSON.Result(
    Result (..),
) where

import Control.Applicative()
import Control.Monad (ap, liftM)
import Control.Monad.Fail()
-- так как монада Result требует описание аппликативного функтора, то
-- здесь написана простая реализация, где fmap = liftM, и pure = Ok, если все выполнилось
-- Возвращает pure и применяет f к a
data Result a = Ok a | Error String
    deriving (Eq, Show)

instance Functor Result where
    fmap = liftM

instance Applicative Result where
    (<*>) :: Result (a -> b) -> Result a -> Result b
    (<*>) = ap
    pure :: a -> Result a
    pure = Ok

instance Monad Result where
    return = pure
    Ok a >>= f = f a
    Error x >>= _ = Error x

instance MonadFail Result where
    fail = Error
