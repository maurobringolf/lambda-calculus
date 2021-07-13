module ChurchEncodingSpec where

import Test.Hspec

import ChurchEncoding.Parser
import ChurchEncoding.Compiler
import ChurchEncoding.TypeChecker.TypeChecker
import ChurchEncoding.TypeChecker.Type(Type(..), equalsUpToRenaming)
import ChurchEncoding.Ast

import qualified Ast as LC
import qualified Parser as LCParser

import Control.Monad.IO.Class

import Data.Map(empty, fromList, singleton)
import qualified Data.Set

testProgramOutput path result = do
  p <- parse <$> (runIO $ readFile path)
  it path $ do
    execTyped p `shouldBe` show result
    exec p `shouldBe` result

testExpType eCode t = do
  let e = parseE eCode
--  runIO $ putStrLn $ show $ inferType e empty
  it (show e ++ "::" ++ show t) $ do
    equalsUpToRenaming (inferType e empty) t `shouldBe` True

testUnificationSuccess eq s = testUnification eq $ Right s

testParseHaskell code eExp = do
  let eAct = parseE code
  it ("parse " ++ show code) $ do
    eAct `shouldBe` eExp

testParseProgram code pExp = do
  let pAct = parse code
  it ("parse " ++ show code) $ do
    pAct `shouldBe` pExp

testUnification eq x = do
  it (show eq) $ case x of
    Right res -> (unify eq `shouldBe` Right (fromList res))
    Left err -> (unify eq `shouldBe` Left err)

main :: IO ()
main = do
  hspec $ do


    describe "(exec . parse) ChurchEncoding programs" $ do

      testProgramOutput "programs/add.hs" (3::Integer)
      testProgramOutput "programs/length.hs" (3::Integer)
      testProgramOutput "programs/cons.hs" [2,1,2,(3::Integer)]
      testProgramOutput "programs/map.hs" [2,4,(6::Integer)]
      testProgramOutput "programs/minus.hs" (0::Integer)
      testProgramOutput "programs/multiline.hs" (4::Integer)
      testProgramOutput "programs/fac.hs" (6::Integer)
      testProgramOutput "programs/ifelse.hs" (9::Integer)
      testProgramOutput "programs/equals.hs" True
      testProgramOutput "programs/true.hs" True
      testProgramOutput "programs/false.hs" False
      testProgramOutput "programs/leq.hs" True
      testProgramOutput "programs/and.hs" True
      testProgramOutput "programs/or.hs" True
      testProgramOutput "programs/infinite.hs" [0::Integer, 1, 2]
      testProgramOutput "programs/polymorph.hs" True
      testProgramOutput "programs/filter.hs" [(7::Integer), 4]

    describe "unify" $ do

      it "equalsUpToRenaming" $ do
        equalsUpToRenaming (TVar 0) (TVar 1) `shouldBe` True
        equalsUpToRenaming TInt TBool `shouldBe` False
        equalsUpToRenaming TInt (TVar 1) `shouldBe` False
        equalsUpToRenaming (TVar 1) (TVar 1) `shouldBe` True
        equalsUpToRenaming (TFun (TVar 1) (TVar 1)) (TFun (TVar 0) (TVar 0)) `shouldBe` True

      testUnificationSuccess (TEQ (TVar 1) (TVar 0)) [(1, TVar 0)]
      testUnificationSuccess (TEQ (TVar 1) TInt) [(1, TInt)]
      testUnificationSuccess (TEQ (TVar 1) (TList TInt)) [(1, TList TInt)]
      testUnificationSuccess (TEQ (TFun TInt TInt) (TVar 0)) [(0, TFun TInt TInt)]
      testUnificationSuccess (TEQ (TFun TInt TInt) (TFun (TVar 0) (TVar 1))) [(0, TInt), (1, TInt)]

      testUnificationSuccess (TEQ (TFun (TVar 0) (TFun (TVar 1) (TVar 0))) (TFun (TFun (TVar 2) (TVar 2)) (TVar 3))) [(0, (TFun (TVar 2) (TVar 2))), (3, (TFun (TVar 1) (TFun (TVar 2) (TVar 2))))]

      testUnificationSuccess (TEQ (TFun (TVar 0) (TVar 0)) (TFun (TVar 1) (TVar 2))) [(0, (TVar 1)), (2, (TVar 1))]

    describe "Typecheck expressions" $ do
      testExpType "1" TInt
      testExpType "1 + 2" TInt
      testExpType "\\x -> x + 2" (TFun TInt TInt)
      testExpType "(\\x -> x + 2) 3" TInt
      testExpType "\\x -> \\a -> a + 1" (TFun (TVar 2) (TFun TInt TInt))
      testExpType "foldr (\\x -> \\a -> a + 1)" (TFun TInt (TFun (TList (TVar 0)) TInt))
      testExpType "foldr (\\x -> \\a -> a + 1) 0" (TFun (TList (TVar 0)) TInt)
      testExpType "\\x -> x" (TFun (TVar 3) (TVar 3))
      testExpType "\\x -> 1" (TFun (TVar 2) TInt)
      testExpType "\\x -> \\y -> x y" (TFun (TFun (TVar 0) (TVar 1)) (TFun (TVar 0) (TVar 1)))
      testExpType "\\x -> x 1" (TFun (TFun TInt (TVar 1)) (TVar 1))
      testExpType "\\x -> x + 1" (TFun TInt TInt)
      testExpType "\\y -> (\\x -> x + 1) y" (TFun TInt TInt)
      testExpType "\\x -> foldr x" (TFun (TFun (TVar 0) (TFun (TVar 1) (TVar 1))) (TFun (TVar 1) (TFun (TList (TVar 0)) (TVar 1))))
      testExpType "\\x -> \\y -> x y" (TFun (TFun (TVar 0) (TVar 1)) (TFun (TVar 0) (TVar 1)))
      testExpType "\\x -> True && (x || x)" (TFun TBool TBool)
      testExpType "[1]" (TList TInt)
      testExpType "2:[1]" (TList TInt)
      testExpType "\\f xs -> foldr (\\x ys -> if f x then x:ys else ys) [] xs" (TFun (TFun (TVar 0) TBool) (TFun (TList (TVar 0)) (TList (TVar 0))))

    describe "ChurchEncoding.parse" $ do

      testParseHaskell "1" (Numeral 1)
      testParseHaskell "1 1" (App (Numeral 1) (Numeral 1))
      testParseHaskell "f x" (App (Var "f") (Var "x"))
      testParseHaskell "f x y" (App (App (Var "f") (Var "x")) (Var "y"))
      testParseHaskell "f (x y)" (App (Var "f") (App (Var "x") (Var "y")))
      testParseHaskell "1 + 1" (App (App Add (Numeral 1)) (Numeral 1)) 
      testParseHaskell "1 * 1" (App (App Mult (Numeral 1)) (Numeral 1)) 
      testParseHaskell "2 + 1" (App (App Add (Numeral 2)) (Numeral 1)) 
      testParseHaskell "x + y + z" (App (App Add (App (App Add (Var "x")) (Var "y"))) (Var "z")) 
      testParseHaskell "f 1 * 2" (App (App Mult (App (Var "f") (Numeral 1))) (Numeral 2)) 
      testParseHaskell "\\x -> x" (Abs "x" (Var "x")) 
      testParseHaskell "\\x -> f x" (Abs "x" (App (Var "f") (Var "x"))) 
      testParseHaskell "\\x -> \\y -> x y" (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) 
      testParseHaskell "\\x -> 1 + 2" (Abs "x" (App (App Add (Numeral 1)) (Numeral 2))) 
      testParseHaskell "\\x -> 2 - (0x23 + 2)" (Abs "x" (App (App Sub (Numeral 2)) (App (App Add (Numeral 35)) (Numeral 2)))) 
      testParseHaskell "\\x y -> x" (Abs "x" (Abs "y" (Var "x")))
      testParseProgram "f x y = y" (P [Def "f" (Abs "x" (Abs "y" (Var "y")))])

    describe "ChurchEncoding.eval" $ do
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

    describe "ChurchEncoding.exec . ChurchEncoding.parse" $ do
      it "exec . parse 'main = 0'" $
        (exec . parse) "main = 0" `shouldBe` (0::Integer)
      it "exec . parse 'main = (\\x -> x + x) 1'" $
        (exec . parse) "main = (\\x -> x + x) 1" `shouldBe` (2::Integer)
      it "exec . parse 'main = (\\x -> x + x) 1 * 3'" $
        (exec . parse) "main = (\\x -> x + x) 1 * 3" `shouldBe` (6::Integer)
