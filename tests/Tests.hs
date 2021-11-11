import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import SimplerEasier
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

singleton = (: [])

genName = singleton <$> Gen.enum 'a' 'z'

genExpr :: Gen Expr
genExpr =
  Gen.recursive
    Gen.choice
    [Var <$> genName]
    [ Gen.subterm2 genExpr genExpr App,
      Gen.subtermM genExpr (\x -> Lam <$> genName <*> pure x)
    ]

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ -- testProperty "whnf preserve freeVars" prop_whnf_preserve_free_vars
      -- testProperty "subst preserve alphaEq" $
      testProperty "arithmetic works" prop_arithmetic
    ]

prop_arithmetic :: Property
prop_arithmetic =
  property $ do
    a <- forAll $ Gen.int (Range.linear 0 100)
    b <- forAll $ Gen.int (Range.linear 0 100)
    assert (betaEq (plus `App` fromIntegral a `App` fromIntegral b) (fromIntegral $ a + b))

-- this is false :sweat_smile:
prop_whnf_preserve_free_vars :: Property
prop_whnf_preserve_free_vars =
  property $ do
      x <- forAll genExpr
      assert (freeVars x == freeVars (whnf x))

-- this is also false, "x" `alphaEq` "y" ?
prop_subs_preserve_alpha_eq :: Property
prop_subs_preserve_alpha_eq =
  property $ do
    x <- forAll genExpr
    s <- forAll genName
    s' <- forAll $ Var <$> genName
    assert (subst s s' x `alphaEq` x)
