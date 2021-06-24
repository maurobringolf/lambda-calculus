module SyntaxSugarSpec where

import Test.Hspec

import SyntaxSugar.Parser
import SyntaxSugar.Compiler
import SyntaxSugar.Ast

import qualified Ast as LC
import qualified Parser as LCParser

import Control.Monad.IO.Class

main :: IO ()
main = do
  add <- liftIO $ readFile "programs/add.hs"
  fac <- liftIO $ readFile "programs/fac.hs"
  ifelse <- liftIO $ readFile "programs/ifelse.hs"
  equals <- liftIO $ readFile "programs/equals.hs"
  true <- liftIO $ readFile "programs/true.hs"
  false <- liftIO $ readFile "programs/false.hs"
  leq <- liftIO $ readFile "programs/leq.hs"
  or <- liftIO $ readFile "programs/or.hs"
  and <- liftIO $ readFile "programs/and.hs"
  multiline <- liftIO $ readFile "programs/multiline.hs"
  minus <- liftIO $ readFile "programs/minus.hs"
  hspec $ do
    describe "(exec . parse) SyntaxSugar programs" $ do
      it "programs/add.hs" $ do
        (exec . parse) add `shouldBe` (3::Integer)

      it "programs/minus.hs" $ do
        (exec . parse) minus `shouldBe` (0::Integer)

      it "programs/multiline.hs" $ do
        (exec . parse) multiline `shouldBe` (4::Integer)

      it "programs/fac.hs" $ do
        (exec . parse) fac `shouldBe` (2::Integer)

      it "programs/ifelse.hs" $ do
        (exec . parse) ifelse `shouldBe` (9::Integer)

      it "programs/equals.hs" $ do
        (exec . parse) equals `shouldBe` True

      it "programs/true.hs" $ do
        (exec . parse) true `shouldBe` True

      it "programs/false.hs" $ do
        (exec . parse) false `shouldBe` False

      it "programs/leq.hs" $ do
        (exec . parse) leq `shouldBe` True

      it "programs/and.hs" $ do
        (exec . parse) and `shouldBe` True

      it "programs/or.hs" $ do
        (exec . parse) or `shouldBe` True

    describe "SyntaxSugar.parse" $ do
      it "parse 'main = 1'" $
        parse "main = 1" `shouldBe` P [Def "main" (Numeral 1)]
      it "parse 'main = 1 1'" $
        parse "main = 1 1" `shouldBe` P [Def "main" (App (Numeral 1) (Numeral 1))]
      it "parse 'main = f x'" $
        parse "main = f x" `shouldBe` P [Def "main" (App (Var "f") (Var "x"))]
      it "parse 'main = f x y'" $
        parse "main = f x y" `shouldBe` P [Def "main" (App (App (Var "f") (Var "x")) (Var "y"))]
      it "parse 'main = f (x y)'" $
        parse "main = f (x y)" `shouldBe` P [Def "main" (App (Var "f") (App (Var "x") (Var "y")))]
      it "parse 'main = 1 + 1'" $
        parse "main = 1 + 1" `shouldBe` P [Def "main" (App (App Add (Numeral 1)) (Numeral 1)) ]
      it "parse 'main = 1 * 1'" $
        parse "main = 1 * 1" `shouldBe` P [Def "main" (App (App Mult (Numeral 1)) (Numeral 1)) ]
      it "parse 'main = 2 + 1'" $
        parse "main = 2 + 1" `shouldBe` P [Def "main" (App (App Add (Numeral 2)) (Numeral 1)) ]
      it "parse 'main = x + y + z'" $
        parse "main = x + y + z" `shouldBe` P [Def "main" (App (App Add (App (App Add (Var "x")) (Var "y"))) (Var "z")) ]
      it "parse 'main = f 1 * 2'" $
        parse "main = f 1 * 2" `shouldBe` P [Def "main" (App (App Mult (App (Var "f") (Numeral 1))) (Numeral 2)) ]
      it "parse 'main = \\x -> x'" $
        parse "main = \\x -> x" `shouldBe` P [Def "main" (Abs "x" (Var "x")) ]
      it "parse 'main = \\x -> f x'" $
        parse "main = \\x -> f x" `shouldBe` P [Def "main" (Abs "x" (App (Var "f") (Var "x"))) ]
      it "parse 'main = \\x -> \\y -> x y'" $
        parse "main = \\x -> \\y -> x y" `shouldBe` P [Def "main" (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) ]
      it "parse 'main = \\x -> 1 + 2'" $
        parse "main = \\x -> 1 + 2" `shouldBe` P [Def "main" (Abs "x" (App (App Add (Numeral 1)) (Numeral 2))) ]
      it "parse 'main = \\x -> 2 - (0x23 + 2)'" $
        parse "main = \\x -> 2 - (0x23 + 2)" `shouldBe` P [Def "main" (Abs "x" (App (App Sub (Numeral 2)) (App (App Add (Numeral 35)) (Numeral 2)))) ]

    describe "SyntaxSugar.eval" $ do
      it "eval '0''" $
        eval (Numeral 0) `shouldBe` (0::Integer)
      it "eval '1''" $
        eval (Numeral 1) `shouldBe` (1::Integer)
      it "eval '2''" $
        eval (Numeral 2) `shouldBe` (2::Integer)
      it "eval '3''" $
        eval (Numeral 3) `shouldBe` (3::Integer)
      it "eval '1 + 1''" $
        eval (App (App Add (Numeral 1)) (Numeral 1)) `shouldBe` (2::Integer)
      it "eval '3 + 1''" $
        eval (App (App Add (Numeral 3)) (Numeral 1)) `shouldBe` (4::Integer)
      it "eval '3 * 1''" $
        eval (App (App Mult (Numeral 3)) (Numeral 1)) `shouldBe` (3::Integer)
      it "eval '0 + 0''" $
        eval (App (App Add (Numeral 0)) (Numeral 0)) `shouldBe` (0::Integer)

    describe "SyntaxSugar.exec . SyntaxSugar.parse" $ do
      it "exec . parse 'main = 0'" $
        (exec . parse) "main = 0" `shouldBe` (0::Integer)
      it "exec . parse 'main = (\\x -> x + x) 1'" $
        (exec . parse) "main = (\\x -> x + x) 1" `shouldBe` (2::Integer)
      it "exec . parse 'main = (\\x -> x + x) 1 * 3'" $
        (exec . parse) "main = (\\x -> x + x) 1 * 3" `shouldBe` (6::Integer)
