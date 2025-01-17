module JSONTransformer (Transformer, field, select, pipe, string, int, equal, elements, greaterThan, greaterThanOrEqual, lessThan, lessThanOrEqual, constructJSON) where

import JSON

-- | A 'Transformer' is a function that takes a single 'JSON' value
-- and returns a list of 'JSON' values.
type Transformer = JSON -> [JSON]

-- HINT: the design of these transformers is based on the design of
-- the Jq tool, and this paper on querying XML:
--
--   "Haskell and XML: generic combinators or type-based translation?"
--   by Malcolm Wallace and Colin Runciman
--   https://dl.acm.org/doi/10.1145/317765.317794
--
-- Section 2 of the paper covers what we're doing. The 'CFilter' type
-- is what we called 'Transformer' here.



-- | Ignores the 'JSON' input and returns the given string as a piece
-- of 'JSON' in a one element list.
--
-- For example,
--
--  > string "hello" (Number 1)
--
-- gives
--
--  > [String "hello"]

string :: String -> Transformer
string s json = [String s]

-- | Ignores the 'JSON' input and returns the given integer as a piece
-- of 'JSON' in a one element list.
--
-- For example,
--
--  > int 1234 (Number 1)
--
-- gives
--
--  > [Number 1234]

int :: Int -> Transformer
int n json = [Number (fromIntegral n)]

-- HINT: these two functions are similar to the 'literal' function in
-- the paper linked above.


-- | Returns all the elements of a `JSON` `Array`. If the `JSON` value
-- is not an `Array`, then it returns the empty list.
--
-- For example,
--
--  > elements (Array [Number 1, Number 2, Boolean True])
--
-- returns
--
--  > [Number 1, Number 2, Boolean True]
--
-- and
--
--  > elements (Number 1)
--
-- returns
--
--  > []
--
-- because 'Number 1' is not an array.
elements :: Transformer
elements jsonObject =
    case getElements jsonObject of
        Just jsons -> jsons
        Nothing -> []

-- HINT: you can use the 'getElements' function from the 'JSON'
-- module.

-- | Looks up a field in a 'JSON' 'Object' and returns the value
-- associated with that field in a list by itself. If the 'JSON' value
-- is not an object, or the field does not exist, then returns the
-- empty list.
--
-- For example,
--
--  > field "a" (Object [("a", Number 1)])
--
-- returns
--
--  > [Number 1]
--
-- and
--
--  > field "b" (Object [("a", Number 1)])
--
-- returns
--
--  > []
--
-- because the field "b" is not in the object.
field :: String -> Transformer
field nm json = case getField nm json of
    Just fieldValue -> [fieldValue]
    Nothing -> []

-- HINT: use 'getField' from the 'JSON' module to define this
-- function.

-- | Connects two transformers together, feeding the output of the
-- first into the input of the second, and then flattening all the
-- results.
--
-- A picture, where 'x' is the input, 'f' is the first transformer,
-- and 'g' is the second.
--
-- @@
--            [v1,   --g--> [[x1,         [x1,
--                            x2],         x2,
-- x  --f-->   v2,   --g-->  [x3,    -->   x3,
--                            x4],         x4,
--             v3]   --g-->  [x5,          x5,
--                            x6]]         x6]
-- @@
pipe :: Transformer -> Transformer -> Transformer
pipe f g json = concatMap g (f json)

-- HINT: this function is the 'o' function in the paper linked above.

-- | Takes two transformers and an input. Applies the input to the two
-- transformers to get two lists of values. Compares all pairs of
-- these values for equality, returning 'Boolean True' or 'Boolean
-- False' for each one.
--
-- For example,
--
--   > equal (string "a") (string "a") (Number 1)
--
-- gives
--
--  > [Boolean True]
--
-- and
--
--  > equal (string "a") (string "b") (Number 1)
--
-- gives
--
--  > [Boolean False]
equal :: Transformer -> Transformer -> Transformer
equal f g json = [Boolean (x == y) | x <- f json, y <- g json]

-- HINT: the easiest way to write this function is to use a list
-- comprehension (Week 4) to get all the pairs returned by the two
-- transformers.

-- | Filter the input. If the transformer argument returns 'true' for
-- the input, then return the input in a single element list. If the
-- transformer argument does not return 'true' then return
select :: Transformer -> Transformer
select f json =
    [json | any isTrue (f json)]

isTrue :: JSON -> Bool
isTrue (Boolean True) = True
isTrue _ = False

-- HINT: you'll need to check to see if the transformer argument
-- returns 'true' at any point in its list. You can use the 'any'
-- function (Week 05) to do this. You'll also need to extract a
-- boolean from the 'JSON' value using 'getBool' in the 'JSON'
-- module. You might want to write a helper function to convert a
-- 'Maybe Bool' to a 'Bool'.

-- ADDITIONAL FEATURES

-- | Filter the input. If the transformer arguemnt returns 'true' for
-- the input, then return the input in a single element list. If the
-- transformer argument does not return 'true' then return

greaterThan :: Transformer -> Transformer -> Transformer
greaterThan f g json = [Boolean (x > y) | x <- f json, y <- g json]

greaterThanOrEqual :: Transformer -> Transformer -> Transformer
greaterThanOrEqual f g json = [Boolean (x >= y) | x <- f json, y <- g json]

lessThan :: Transformer -> Transformer -> Transformer
lessThan f g json = [Boolean (x < y) | x <- f json, y <- g json]

lessThanOrEqual :: Transformer -> Transformer -> Transformer
lessThanOrEqual f g json = [Boolean (x <= y) | x <- f json, y <- g json]


-- | Constructs a JSON object using a list of transformers. Each 
-- transformer is applied to the input JSON, and the results are
-- flattened into a list of key -value pairs in the final JSON object.

constructJSON :: [Transformer] -> Transformer
constructJSON transformers json = [Object results]
  where
    results = concat [getObjectFields (transformer json) | transformer <- transformers]

getObjectFields :: [JSON] -> [(String, JSON)]
getObjectFields (Object fields : _) = fields
getObjectFields _ = []



