module CSV where

-- | Using LiquidHaskell for CSV lists
-- c.f. http://www.reddit.com/r/scala/comments/1nhzi2/using_shapelesss_sized_type_to_eliminate_real/



-- | Example

--     Item        Price
--     ----        -----
--     Espresso    2.25
--     Macchiato   2.75
--     Cappucino   3.35
--     Americano   2.25

-- | Type 

data CSV = Csv { headers :: [String]
               , rows    :: [[String]]
               }

-- | Table 

zumbarMenu = Csv [  "Item"     , "Price"]
                 [ ["Espresso" , "2.25" ]
                 , ["Macchiato", "2.75" ]
                 , ["Cappucino", "3.35" ]
                 , ["Americano", "2.25" ]
                 ]

-- | Off By One Errors

-- Eeks, we missed the header name!

csvBad1 = Csv [  "Date" {- ??? -} ]
              [ ["Mon", "1"]
              , ["Tue", "2"]
              , ["Wed", "3"]
              ]

-- Blergh! we missed a column.

csvBad2 = Csv [  "Name" , "Age"  ]
              [ ["Alice", "32"   ]
              , ["Bob"  {- ??? -}]
              , ["Cris" , "29"   ]
              ]

-- | Invariant

-- Want to *refine* the `CSV` type to specify that the *number* of
-- elements in each row is *exactly* the same as the   *number* of headers.

{-@ data CSV = Csv { headers :: [String]
                   , rows    :: [{v:[String] | (len v) = (len headers)}]
                   }
  @-}


-- All is well!

csvGood = Csv ["Id", "Name", "Days"]
              [ ["1", "Jan", "31"]
              , ["2", "Feb", "28"]
              , ["3", "Mar", "31"]
              , ["4", "Apr", "30"]
              ]


-- | Bonus Points

-- How would you modify the specification to prevent table with 
-- degenerate entries like this one?

deg = Csv [  "Id", "Name", "Days"]
          [ ["1" , "Jan" , "31"]
          , ["2" , "Feb" , ""]
          ]
