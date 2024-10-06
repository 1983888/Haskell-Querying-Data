module QueryLanguage where

import JSONTransformer
import JSON

data Query
  = Pipe        Query Query
  | Field       String
  | Elements
  | Select      Query
  | ConstInt    Int
  | ConstString String
  | Equal       Query Query
  | GreaterThan Query Query
  | GreaterThanOrEqual Query Query
  | LessThan Query Query
  | LessThanOrEqual Query Query
  | ConstructJSON [Query] 
  deriving Show

-- | Executes a 'Query' by translating it into a `Transformer`. Each
-- of the constructors of 'Uery' is turned into its corresponding
-- `Transformer` defined in `JSONTransformer`.
--
-- For example:
--
-- >  execute Elements (Array [Number 1, Number 2])
--
-- returns
--
-- >  [Number 1, Number 2]
--
-- which is the behaviour of `elements` on this input.
execute :: Query -> Transformer
execute (Pipe q1 q2)               = pipe (execute q1) (execute q2)
execute (Field fieldName)          = field fieldName
execute Elements                   = elements
execute (Select q)                 = select (execute q)
execute (ConstInt n)               = int n
execute (ConstString s)            = string s
execute (Equal q1 q2)              = equal (execute q1) (execute q2)
execute (GreaterThan q1 q2)        = greaterThan (execute q1) (execute q2)
execute (GreaterThanOrEqual q1 q2) = greaterThanOrEqual (execute q1) (execute q2)
execute (LessThan q1 q2)           = lessThan (execute q1) (execute q2)
execute (LessThanOrEqual q1 q2)    = lessThanOrEqual (execute q1) (execute q2)
execute (ConstructJSON queries)    = constructJSON (map execute queries)

-- HINT: this function is very similar to the 'eval' function for
-- evaluating Boolean formulas defined in the Week03 problems.
