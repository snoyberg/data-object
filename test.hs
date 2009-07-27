import Data.Object

test = toObject
    [ ("foo", ["bar", "baz"])
    , ("bin", ["bin1", "bin2"])
    ]
main = do
    print test
    bin <- fromObject test >>= oLookup "bin" :: IO [String]
    print bin
