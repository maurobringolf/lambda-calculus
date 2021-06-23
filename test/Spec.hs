import Test.Hspec

import Interpreter
import Parser
import Ast

import qualified SyntaxSugarSpec

runLazy :: String -> Term
runLazy = lazyEval . parse

runEager :: String -> Term
runEager = eagerEval . parse

main :: IO ()
main = do
  hspec $
    describe "LambdaCalculus.lazyEval" $ do

      it "parse λx.x" $
        parse "λx.x" `shouldBe` (Abs "x" (Var "x"))

      it "parse λx  .  x" $
        parse "λx  .  x" `shouldBe` (Abs "x" (Var "x"))

      it "parse λx.λy.x y" $
        parse "λx.λy.x y" `shouldBe` (Abs "x" (
          (Abs "y"
            (App (Var "x") (Var "y"))
          )
        ))

      it "parse (λx.λy.x) y" $
        parse "(λx.λy.x) y" `shouldBe` (App
          (Abs "x" (Abs "y" (Var "x")))
          (Var "y")
        )

      it "show 'λx.λy.x y'" $
        show (Abs "x" (
          (Abs "y"
            (App (Var "x") (Var "y"))
          )
        )) `shouldBe` "λx.λy.x y"

      it "show 'x y'" $
        show (App (Var "x") (Var "y")) `shouldBe` "x y"

      it "show 'x y z'" $
        show (App (App (Var "x") (Var "y")) (Var "z")) `shouldBe` "x y z"

      it "show 'x y λz.z'" $
        show (App (App (Var "x") (Var "y")) (Abs "z" (Var "z"))) `shouldBe` "x y λz.z"

      it "show '(λx.x) y'" $
        show (App (Abs "x" (Var "x")) (Var "y")) `shouldBe` "(λx.x) y"

      it "show 'λx.x y'" $
        show (Abs "x" (App (Var "x") (Var "y"))) `shouldBe` "λx.x y"

      it "runLazy (λx.(λy.y) x)" $
        runLazy "(λx.x) y" `shouldBe` parse "y"

      it "runEager (λx.(λy.y) x)" $
        runEager "(λx.(λy.y) x)" `shouldBe` parse "λx.x"

      it "runLazy (λx.(λy.y) x)" $
        runLazy "(λx.(λy.y) x)" `shouldBe` parse "(λx.(λy.y) x)" 

      it "runLazy (λx.x) y" $
        runLazy "(λx.x) y" `shouldBe` parse "y"

      it "runLazy (λx.x) x" $
        runLazy "(λx.x) x" `shouldBe` parse "x"

      it "runLazy (λx.y) x" $
        runLazy "(λx.y) x" `shouldBe` parse "y"
        
      it "runLazy (λx.λy.x y) y" $
        runLazy "(λx.λy.x y) y" `shouldBe` parse "λz.y z"

      it "runLazy (λx.λy.x y) y x" $
        runLazy "(λx.λy.x y) y x" `shouldBe` parse "y x" 

      it "runLazy (λx.λy.x)(λz.z) --> λy.λz.z" $
        runLazy "(λx.λy.x)(λz.z)" `shouldBe` parse "λy.λz.z"

      it "runLazy (λx.x (λy. x y)) (λx. (λy. y) x) --> (λy. (λx. (λy.y) x) y) " $
        runLazy "(λx.x (λy. x y)) (λx. (λy. y) x)" `shouldBe` parse "λy. (λx. (λy.y) x) y"

      it "runEager (λx.x (λy. x y)) (λx. (λy. y) x) --> (λy.y)" $
        runEager "(λx.x (λy. x y)) (λx. (λy. y) x)" `shouldBe` parse "λy.y"

      it "parse y combinator part 1" $
        parse "λx.(f (x x))" `shouldBe` Abs "x" (App
          (Var "f")
          (App (Var "x") (Var "x"))
        )

      it "parse y combinator part 2" $
        parse "λf.(λx.z z)" `shouldBe` Abs "f" (Abs "x"
          (App (Var "z") (Var "z"))
        )

      it "parse y combinator part 3" $
        parse "λf.(λx.z z)" `shouldBe` Abs "f" (Abs "x"
          (App (Var "z") (Var "z"))
        )
  SyntaxSugarSpec.main
