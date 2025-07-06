{-# LANGUAGE OverloadedStrings #-}
module MyCodeGenSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Data.Either (fromRight)
import MyParser (parseMyLang)
import MyCodeGen (compileAST)
import Sprockell (Instruction(..), numberIO)

spec :: Spec
spec = describe "MyCodeGen.compileAST" $ do
  it "appends EndProg even for empty program" $ do
    compileAST [] `shouldBe` [EndProg]

  it "generates load and store for integer declaration" $ do
    let ast    = fromRight [] $ parseMyLang "i x;"
        instrs = compileAST ast
    take 2 instrs `shouldSatisfy` all isLoadOrStore

  it "generates store for boolean declaration" $ do
    let ast    = fromRight [] $ parseMyLang "am b;"
        instrs = compileAST ast
    instrs `shouldSatisfy` any isStore

  it "generates load and store for assignment literal" $ do
    let ast    = fromRight [] $ parseMyLang "i x; x = 5;"
        instrs = compileAST ast
    take 2 (drop 2 instrs) `shouldSatisfy` all isLoadOrStore

  it "generates compute and store for arithmetic precedence" $ do
    let ast    = fromRight [] $ parseMyLang "i x; x = 1+2*3;"
        instrs = compileAST ast
    instrs `shouldSatisfy` any isCompute
    instrs `shouldSatisfy` any isStore

  it "throws on unsupported string literal codegen" $ do
    let ast = fromRight [] $ parseMyLang "sci s = \"A\"; print \"A\";"
    evaluate (last $ compileAST ast) `shouldThrow` anyErrorCall

  it "generates code for print integer" $ do
    let ast    = fromRight [] $ parseMyLang "print 3;"
        instrs = compileAST ast
    instrs `shouldSatisfy` any isWriteNum

  it "generates branch for if-then-else" $ do
    let ast    = fromRight [] $ parseMyLang "major(true){ x=1;} having { x=0;}"
        instrs = compileAST ast
    instrs `shouldSatisfy` any isBranch

  it "generates jump for while loop" $ do
    let ast    = fromRight [] $ parseMyLang "fun(true){ x=1;}"
        instrs = compileAST ast
    instrs `shouldSatisfy` any isJump

-- Helpers
isLoadOrStore :: Instruction -> Bool
isLoadOrStore Load{}  = True
isLoadOrStore Store{} = True
isLoadOrStore _       = False

isStore :: Instruction -> Bool
isStore Store{} = True
isStore _       = False

isCompute :: Instruction -> Bool
isCompute (Compute _ _ _ _) = True
isCompute _                 = False

isWriteNum :: Instruction -> Bool
isWriteNum (WriteInstr _ io) = io == numberIO
isWriteNum _                 = False

isBranch :: Instruction -> Bool
isBranch Branch{} = True
isBranch _        = False

isJump :: Instruction -> Bool
isJump Jump{} = True
isJump _      = False
