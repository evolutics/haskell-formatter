
isFixedPoint :: (Eq a) => (a -> a) -> a -> Bool
isFixedPoint function argument = function argument == argument
