module JSONOutput where

import JSON
import Data.List (intersperse)

-- | Converts a 'String' into its string JSON representation by
-- placing quotes around it and escaping any quote characters that
-- appear in the string.
--
-- For example,
--
-- >  renderString "hello \"world\""
--
-- Returns
--
-- >  "\"hello \\\"world\\\"\""
renderString :: String -> String
renderString str = "\"" ++ escapeQuotes str ++ "\""
  where
    escapeQuotes [] = []
    escapeQuotes ('"':xs) = '\\' : '"' : escapeQuotes xs
    escapeQuotes (x:xs) = x : escapeQuotes xs
    

-- HINT: one way of doing the escaping is the same way that we did the
-- htmlEscape function in the Week01 problems.

-- | Converts `JSON` into its string representation, as documented in
-- the JSON standard (https://www.json.org/json-en.html). The format
-- should be as is understood by the parsing functions in `JSONInput`.
--
-- For example,
--
--  > renderJSON (Object [("a",Number 1), ("b",Number 2)])
--
-- should give
--
--  > "{\"a\": 1, \"b\": 2}"
renderJSON :: JSON -> String
renderJSON json = case json of 
  Null -> "null"
  Boolean True -> "true"
  Boolean False -> "false"
  Number n -> show n
  String s -> renderString s
  Array arr -> "[" ++ renderArray arr ++ "]"
  Object obj -> "{" ++ renderObject obj ++ "}"

renderArray :: [JSON] -> String
renderArray = concat . intersperse ", " . map renderJSON

renderObject :: [(String, JSON)] -> String
renderObject = concat . intersperse ", " . map renderPair
  where
    renderPair (key, value) = renderString key ++ ": " ++ renderJSON value

-- HINT: the `intersperse` function (imported above) is a good way of
-- putting something between every element of a list. It is the
-- "printing" counterpart of the 'sepBy' parser combinator.
--
-- You should use 'renderString' to implement rendering of 'String's
-- in the JSON.

-- ADDITIONAL FEATURES (Will only work in ghci, did'nt add to main as couldnt figure it out)

-- | Converts a 'String' into its CSV representation.

renderCSV :: JSON -> String
renderCSV json = case json of
  Null -> "null"
  Boolean True -> "true"
  Boolean False -> "false"
  Number n -> show n
  String s -> s
  Array arr -> renderArrayCSV arr
  Object obj -> renderObjectCSV obj

renderArrayCSV :: [JSON] -> String
renderArrayCSV = concat . intersperse "," . map renderCSV

renderObjectCSV :: [(String, JSON)] -> String
renderObjectCSV = concat . intersperse "," . map renderPairCSV
  where
    renderPairCSV (key, value) = key ++ "," ++ renderCSV value

-- | Converts a 'JSON' into its HTML representation 

renderHTML :: JSON -> String
renderHTML json = case json of
  Null -> "<div>null</div>"
  Boolean True -> "<div>true</div>"
  Boolean False -> "<div>false</div>"
  Number n -> "<div>" ++ show n ++ "</div>"
  String s -> "<div>" ++ s ++ "</div>"
  Array arr -> "<div>" ++ renderArrayHTML arr ++ "</div>"
  Object obj -> "<div>" ++ renderObjectHTML obj ++ "</div>"

renderArrayHTML :: [JSON] -> String
renderArrayHTML = concatMap (\item -> "<div>" ++ renderHTML item ++ "</div>")

renderObjectHTML :: [(String, JSON)] -> String
renderObjectHTML = concatMap (\(key,value) -> "<div>" ++ key ++ ": " ++ renderHTML value ++ "</div>")


