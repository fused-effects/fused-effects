{-# LANGUAGE DeriveFunctor, KindSignatures #-}
module Parser where

data Symbol (m :: * -> *) k = Symbol Char (Char -> k)
  deriving (Functor)
