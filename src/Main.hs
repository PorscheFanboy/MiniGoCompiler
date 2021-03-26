module Main where

import Data.Char
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import System.IO (isEOF, stderr, hPutStrLn, openFile, IOMode(..), hGetContents, hClose)
import System.Environment (getArgs)
import System.FilePath(splitExtension, addExtension)
import System.Exit (exitWith, ExitCode(..))
import Text.Parsec 

import Tokenizer (tokenize, formatError, TokenInfo(..))
import PrettyPrinter (prettyPrint)
import Parser (parseTokens, formatError, Program(..))
import Weeder (weed, WeedError)
import Typechecker (symbolTable, SymbolTable(..), printSymbolTable, getProgram)
import Codegen (generateCode)

-- Show the AST when parsing - useful for debugging
showAst = False

-- Read the input and return it as an array of tokens
-- or print an error and exit if invalid.
readTokens :: IO String -> MaybeT IO [TokenInfo]
readTokens src =  do
  inp <- lift src
  case tokenize inp of
    Left error   -> lift (exitWithError $ Tokenizer.formatError error) *> MaybeT (return Nothing)
    Right tokens -> MaybeT $ return $ Just tokens

readProgram :: [TokenInfo] -> MaybeT IO Program
readProgram tokens =
  case parseTokens tokens of
    Left error -> lift (exitWithError $ Parser.formatError error) *> MaybeT (return Nothing)
    Right prog -> case weed prog of
      Left error -> lift (exitWithError $ "Error: " ++ error) *> MaybeT (return Nothing)
      Right () -> MaybeT $ return $ Just prog

readTable :: Program -> MaybeT IO SymbolTable
readTable prog = 
  case symbolTable prog of
    Left error  -> lift (exitWithError $ "Error: " ++ show error) *> MaybeT (return Nothing)
    Right table -> MaybeT $ return $ Just table


-- Print an error message and exit
exitWithError :: String -> IO ()
exitWithError e = hPutStrLn stderr e *> exitWith (ExitFailure 1)

usageString :: String
usageString = "Usage: minilang <scan|tokens|parse|pretty|symbol|typecheck|codegen> [path]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cmd]       -> runCmd cmd getContents []
    [cmd, path] -> do file <- openFile path ReadMode
                      runCmd cmd (hGetContents file) path
    otherwise   -> putStrLn usageString
  return ()
  where
    runCmd :: String -> IO String -> String -> IO ()
    runCmd cmd src path = case cmd of 
      "tokens"    -> mainTokens src *> return ()
      "scan"      -> mainScan src *> return ()
      "parse"     -> mainParse src *> return ()
      "pretty"    -> mainPretty src *> return ()
      "typecheck" -> mainTypeCheck src *> return ()
      "symbol"    -> mainSymbol src *> return ()
      "codegen"   -> mainCodegen src path *> return ()
      otherwise   -> putStrLn usageString

mainScan :: IO String -> IO (Maybe ())
mainScan src = runMaybeT $ do
  tokens <- readTokens src
  lift $ putStrLn "OK"

mainTokens :: IO String -> IO (Maybe ())
mainTokens src = runMaybeT $ do
  tokens <- readTokens src
  lift $ mapM_ (\(TokenInfo tok pos) -> putStrLn $ show $ tok) tokens

mainPretty :: IO String -> IO (Maybe ())
mainPretty src = runMaybeT $ do
  prog <- readTokens src >>= readProgram
  lift $ putStrLn (prettyPrint prog)

mainParse :: IO String -> IO (Maybe ())
mainParse src = runMaybeT $ do
  prog <- readTokens src >>= readProgram
  if showAst
    then lift $ putStrLn $ show prog
    else lift $ putStrLn "OK"

mainSymbol :: IO String -> IO (Maybe ())
mainSymbol src = runMaybeT $ do
  table <- readTokens src >>= readProgram >>= readTable
  lift $ putStrLn $ printSymbolTable table

mainTypeCheck :: IO String -> IO (Maybe ())
mainTypeCheck src = runMaybeT $ do
  table <- readTokens src >>= readProgram >>= readTable
  lift $ putStrLn $ "OK"

mainCodegen :: IO String -> String -> IO (Maybe ())
mainCodegen src path = runMaybeT $ do
  prog <- readTokens src >>= readProgram
  table <- readTable prog
  lift $ writeFile path' (generateCode (getProgram table))
  lift $ putStrLn $ "OK"
  where
    (f, ext_) = splitExtension path
    path' = addExtension f "asm"
