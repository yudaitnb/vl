module Syntax.Substitution (SubstMap) where

import Data.Map

import Syntax.Type

type SubstMap = Map String Type