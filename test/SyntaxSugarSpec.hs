module SyntaxSugarSpec where

import Test.Hspec

import SyntaxSugar.Parser
import SyntaxSugar.Ast

main :: IO ()
main = hspec $
  describe "SyntaxSugar" $ do

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

