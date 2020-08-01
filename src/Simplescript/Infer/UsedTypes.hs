module Simplescript.Infer.UsedTypes where

import Data.Text

type VarName = Text

data Qualified
    = Qualified ModuleName

type ModuleName = Text

type Binding expr = (VarName, expr)
