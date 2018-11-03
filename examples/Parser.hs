{-# LANGUAGE KindSignatures #-}
module Parser where

data Symbol (m :: * -> *) k = Symbol Char (Char -> k)
