import Test.Framework (defaultMain)

import qualified Data.Object

main :: IO ()
main = defaultMain
    [ Data.Object.testSuite
    ]
