{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Framework (defaultMain)

import qualified Data.Object.Text

main :: IO ()
main = defaultMain
    [ Data.Object.Text.testSuite
    ]
