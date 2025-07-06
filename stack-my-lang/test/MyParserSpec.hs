{-# LANGUAGE OverloadedStrings #-}
module MyParserSpec (spec) where

import Test.Hspec
import MyParser (parseMyLang)

spec :: Spec
spec = describe "MyParser.parseMyLang" $ do
  it "parses integer declaration" $ do
    show (parseMyLang "i x;") `shouldBe` "Right [Decl TypeInt \"x\"]"

  it "parses boolean declaration" $ do
    show (parseMyLang "am y;") `shouldBe` "Right [Decl TypeBool \"y\"]"

  it "parses string declaration" $ do
    show (parseMyLang "sci s = \"hello\";") `shouldBe` "Right [DeclStr \"s\" \"hello\"]"

  it "parses vector declaration" $ do
    show (parseMyLang "sci v = [1,2,3];") `shouldBe` "Right [DeclVec \"v\" [IntLit 1,IntLit 2,IntLit 3]]"

  it "parses assignment of literal" $ do
    show (parseMyLang "x = 42;") `shouldBe` "Right [Assign \"x\" (IntLit 42)]"

  it "parses print statement" $ do
    show (parseMyLang "print x;") `shouldBe` "Right [Print (Var \"x\")]"

  it "parses boolean literal as identifier" $ do
    show (parseMyLang "print true;") `shouldBe` "Right [Print (Var \"true\")]"



  it "respects operator precedence (* over +)" $ do
    show (parseMyLang "print 1+2*3;") `shouldBe`
      "Right [Print (BinOp \"+\" (IntLit 1) (BinOp \"*\" (IntLit 2) (IntLit 3)))]"

  it "parses if without else" $ do
    show (parseMyLang "major(x<1){x=1;}") `shouldBe`
      "Right [If (BinOp \"<\" (Var \"x\") (IntLit 1)) [Assign \"x\" (IntLit 1)] Nothing]"

  it "parses if with else" $ do
    show (parseMyLang "major(x<1){x=1;} having { x=2; }") `shouldBe`
      "Right [If (BinOp \"<\" (Var \"x\") (IntLit 1)) [Assign \"x\" (IntLit 1)] (Just [Assign \"x\" (IntLit 2)])]"

  it "parses while loop" $ do
    show (parseMyLang "fun(x!=0){x=x-1;}") `shouldBe`
      "Right [While (BinOp \"!=\" (Var \"x\") (IntLit 0)) [Assign \"x\" (BinOp \"-\" (Var \"x\") (IntLit 1))]]"

  it "parses fork/join" $ do
    show (parseMyLang "fork{i x;} join;") `shouldBe`
      "Right [ForkJoin [Decl TypeInt \"x\"]]"

  it "parses lock block" $ do
    show (parseMyLang "lock L {x=0;} ") `shouldBe`
      "Right [Lock \"L\" [Assign \"x\" (IntLit 0)]]"

  it "parses nested block" $ do
    show (parseMyLang "{i x; print x;}" ) `shouldBe`
      "Right [Block [Decl TypeInt \"x\",Print (Var \"x\")]]"

  it "rejects invalid syntax" $ do
    parseMyLang "foo" `shouldSatisfy` (\r -> case r of Left _ -> True; _ -> False)
