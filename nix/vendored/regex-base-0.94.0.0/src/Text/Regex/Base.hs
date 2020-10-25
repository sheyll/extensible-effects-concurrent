{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
--
-- Module      :  Text.Regex.Base
-- Copyright   :  (c) Chris Kuklewicz 2006
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Maintainer  :  hvr@gnu.org
-- Stability   :  experimental
-- Portability :  non-portable (MPTC+FD)
--
-- Classes and instances for Regex matching.
--
--
-- This module merely imports and re-exports the common part of the new
-- api: "Text.Regex.Base.RegexLike" and "Text.Regex.Base.Context".
--
-- To see what result types the instances of RegexContext can produce,
-- please read the "Text.Regex.Base.Context" haddock documentation.
--
-- This does not provide any of the backends, just the common interface
-- they all use.  The modules which provide the backends and their cabal
-- packages are:
--
--  * @Text.Regex.Posix@ from regex-posix
--
--  * @Text.Regex@ from regex-compat (uses regex-posix)
--
--  * @Text.Regex.Parsec@ from regex-parsec
--
--  * @Text.Regex.DFA@ from regex-dfa
--
--  * @Text.Regex.PCRE@ from regex-pcre
--
--  * @Test.Regex.TRE@ from regex-tre
--
-- In fact, just importing one of the backends is adequate, you do not
-- also need to import this module.
--
-- == Example
--
-- The code below
--
-- @
-- import Text.Regex.Base
-- import Text.Regex.Posix((=~),(=~~)) -- or DFA or PCRE or PosixRE
--
-- main = let b :: Bool
--            b = ("abaca" =~ "(.)a")
--            c :: [MatchArray]
--            c = ("abaca" =~ "(.)a")
--            d :: Maybe (String,String,String,[String])
--            d = ("abaca" =~~ "(.)a")
--        in do print b
--              print c
--              print d
-- @
--
-- will output
--
-- > True
-- > [array (0,1) [(0,(1,2)),(1,(1,1))],array (0,1) [(0,(3,2)),(1,(3,1))]]
-- > Just ("a","ba","ca",["b"])
--

-----------------------------------------------------------------------------

module Text.Regex.Base (getVersion_Text_Regex_Base
  -- | RegexLike defines classes and type, and 'Extract' instances
  ,module Text.Regex.Base.RegexLike) where

import Data.Version(Version(..))
import Text.Regex.Base.RegexLike
import Text.Regex.Base.Context()

import qualified Paths_regex_base

getVersion_Text_Regex_Base :: Version
getVersion_Text_Regex_Base = Paths_regex_base.version
