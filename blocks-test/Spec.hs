{-# LANGUAGE TemplateHaskell #-}
import           Blocks
import           Test.QuickCheck

instance Arbitrary Blok where
  arbitrary = do
    x       <- arbitrary
    y       <- arbitrary
    breedte <- arbitrary
    hoogte  <- arbitrary
    diepte  <- arbitrary
    return $ Blok x y breedte hoogte diepte

prop_LeftRight b = left (right b) == b
  where types = b::Blok

return []
runTests = $quickCheckAll

-- main :: IO ()
main = runTests

