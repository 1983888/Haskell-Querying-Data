import System.Exit
import Test.HUnit
import JSON
import JSONTransformer
import JSONOutput
import QueryLanguage

-- Function for comparing two lists of JSON values 
assertJsonEqual :: [JSON] -> [JSON] -> Assertion
assertJsonEqual = assertEqual "JSON values not equal"

testString :: Test
testString = TestList
    [ TestCase $ assertJsonEqual [String "hello"] (string "hello" (Number 1))
    -- Tests string equality with a single word string

    , TestCase $ assertJsonEqual [String ""] (string "" (Number 1))
    -- Tests string equality with an ampty string

    , TestCase $ assertJsonEqual [String "!£$%&*()"] (string "!£$%&*()" (Number 1))
    -- Tests string equality with a string containing special characters
    ]
 
testInt :: Test
testInt = TestList
    [ TestCase $ assertJsonEqual [Number 1234] (int 1234 (Number 1))
    -- Tests integer equality
    , TestCase $ assertJsonEqual [Number (-5)] (int (-5) (Number 1))
    -- Tests integer equality with negative numbers
    , TestCase $ assertJsonEqual [Number 0] (int 0 (Number 1))
    -- Tests integer equality with zero
    ]

testElements :: Test
testElements = TestList
    [ TestCase $ assertJsonEqual [Number 1, Number 2, Boolean True] (elements (Array [Number 1, Number 2, Boolean True]))
    -- Tests elements function with an array of numbers and a boolean

    , TestCase $ assertJsonEqual [Number 1, String "hello", Boolean True] (elements (Array [Number 1, String "hello", Boolean True]))
    -- Tests elements function with an array of number, string, and boolean values
    ]

testField :: Test
testField = TestList
    [ TestCase $ assertJsonEqual [Number 1] (field "a" (Object [("a", Number 1)]))
    -- Tests field extraction with a single key value pair
    , TestCase $ assertJsonEqual [Number 42] (field "answer" (Object[("question", String "How many planets are there"), ("answer", Number 42)]))
    -- Tests field extraction with a complex object and nested structure 
    ]
  
testPipe :: Test
testPipe = TestList
    [ TestCase $ assertJsonEqual [Number 1,Number 2,Number 3,Number 4] (pipe elements elements (Array [Array [Number 1,Number 2], Array [Number 3,Number 4]]))
    --Tests if the pipe function correclty flattens two levels of arrays

    , TestCase $ assertJsonEqual [Number 1, Number 2] (pipe elements (field "a") (Array [Object [("a",Number 1)], Object [("a", Number 2)], Object []]))
    --Tests if the pipe function correclty takes everything from an array and then all the `a` fields from the objects

    , TestCase $ assertJsonEqual [Number 1, String "abc", Null] (pipe (field "a") elements (Object [("a", Array [Number 1, String "abc", Null])]))
    --Tests if the pipe function correclty looks up the field `a` in an object and then gets all the elements from the array stored in that field

    , TestCase $ assertJsonEqual [] (pipe (equal (int 1) (int 2)) elements (Array []))
    --Tests if the pipe function works correclty with an empty array as input
    ]
    

testEqual :: Test
testEqual = TestList
    [ TestCase $ assertJsonEqual [Boolean True] (equal (string "a") (string "a") (Number 1))
    -- Tests if two equal strings produce Boolean True

    , TestCase $ assertJsonEqual [Boolean True] (equal (int 1) (int 1) Null) 
    -- Tests if two equal integers produce Boolean True

    , TestCase $ assertJsonEqual [Boolean False] (equal (int 1) (int 2) Null)
    -- Tests if two unequal integers produce Boolean False

    , TestCase $ assertJsonEqual [Boolean True, Boolean False, Boolean False, Boolean False] (equal elements (int 1) (Array [Number 1, Number 2, Number 3, Number 4]))
    -- Tests comparing the elements of an array to a fixed integer

    , TestCase $ assertJsonEqual [Boolean True, Boolean False, Boolean False, Boolean True] (equal elements elements (Array [Number 1, Boolean True]))
    -- Tests if elements in an array are equal to each other, producing multiple booleans

    , TestCase $ assertJsonEqual [Boolean True] (equal (field "a") (string "X") (Object [("a", String "X")]))
    -- Tests if a field in an object is equal to a string, producing Boolean True

    , TestCase $ assertJsonEqual [Boolean False] (equal (field "a") (string "X") (Object [("a", String "Y")]))
    -- Tests if a field in an object is equal to a different string, producing Boolean False
    ]
 
testSelect :: Test
testSelect = TestList
    [ TestCase $ assertJsonEqual [Array [Number 1, Number 2]] (select (equal (int 1) (int 1)) (Array [Number 1, Number 2]))
    -- Tests if selecting based on equal integers produces the correct array

    , TestCase $ assertJsonEqual [] (select (equal (int 1) (int 2)) (Array [Number 1, Number 2]))
    -- Tests if selecting based on different integers produces an empty array

    , TestCase $ assertJsonEqual [Object [("a", Number 1)]] (select (equal (field "a") (int 1)) (Object [("a", Number 1)]))
    -- Tests if selecting based on equal integer field values produces the correct object

    , TestCase $ assertJsonEqual [] (select (equal (field "a") (int 1)) (Object [("a", Number 2)]))
    -- Tests if selecting based on different integer field values produces an empty array

    , TestCase $ assertJsonEqual [Array [Number 1, Number 3, Number 4]] (select (equal elements (int 1)) (Array [Number 1, Number 3, Number 4]))
    -- Tests if the `equal` returns multiple values, then only one of them needs to be `True` for it to select that thing, so we can check to see if a certain element is in an array

    , TestCase $ assertJsonEqual [] (select (equal elements (int 1)) (Array [Number 3, Number 4]))
    -- Tests if selecting based on different integer elements produces an empty array

    , TestCase $ assertJsonEqual [Object [("a", Array [Number 1, String "abc", Null])]] (select (equal (pipe (field "a") elements) (int 1)) (Object [("a", Array [Number 1, String "abc", Null])]))
    -- Tests if selecting based on equal piped integer elements produces the correct object

    , TestCase $ assertJsonEqual [] (select (equal (pipe (field "a") elements) (int 1)) (Object [("a", Array [Number 2, String "abc", Null])]))
    -- Tests if selecting based on different pipe integer elements produces an empty array
    ]
    
testRenderString :: Test
testRenderString = TestList
    [ TestCase $ assertEqual "Render string JSON representation" "\"hello \\\"world\\\"\"" (renderString "hello \"world\"")
    -- Tests rendering a string with escaped quotes

    , TestCase $ assertEqual "Render string JSON representation" "\"\"" (renderString "")
    --Tests rendering an empty string
    ]
    
testRenderJSON :: Test
testRenderJSON = TestList
    [ TestCase $ assertEqual "Render JSON object representation" "{\"a\": 1, \"b\": 2}" (renderJSON (Object [("a", Number 1), ("b", Number 2)]))
    -- Tests rendering an object with numeric and string values

    , TestCase $ assertEqual "Render JSON null" "null" (renderJSON Null)
    -- Tests rendering a null value

    , TestCase $ assertEqual "Render JSON boolean" "true" (renderJSON (Boolean True))
    -- Tests rendering a boolean value

    , TestCase $ assertEqual "Render JSON number" "42" (renderJSON (Number 42))
    -- Tests rendering a numeric value

    , TestCase $ assertEqual "Render JSON string" "\"hello world\"" (renderJSON (String "hello world"))
    -- Tests rendering a string value

    , TestCase $ assertEqual "Render JSON array" "[1, 2, 3]" (renderJSON (Array [Number 1, Number 2, Number 3]))
    --Tests rendering an array with numeric values

    , TestCase $ assertEqual "Render JSON nested" "{\"key\": [1, \"value\", {\"nested\": true}]}" (renderJSON (Object [("key", Array [Number 1, String "value", Object [("nested", Boolean True)]] )]))
    -- Tests rendering a nested JSON structure

    , TestCase $ assertEqual "Render JSON empty array" "[]" (renderJSON (Array []))
    -- Tests rendering an empty array

    , TestCase $ assertEqual "Render JSON empty object" "{}" (renderJSON (Object []))
    -- Tests rendering an empty object
    ]
    
main :: IO ()
main = do
    counts <- runTestTT $ TestList
        [ testString
        , testInt
        , testElements
        , testField
        , testPipe
        , testEqual
        , testSelect
        , testRenderString
        , testRenderJSON
        ]

    if errors counts == 0 && failures counts == 0
        then exitSuccess
        else exitFailure



