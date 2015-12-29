{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MonadBot.Plugins.Brainfunk
    ( runProgram
    , Brainfunk (..)
    )
    where

import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Char

import MonadBot.Plugins.ListZipper

type Buffer = ListZipper Word8
type Program = ListZipper Char
type Output = String


data BFContext
    = BFContext
    { program :: Program
    , buffer :: Buffer
    , output :: String
    } deriving (Show)

newtype Brainfunk a
    = Brainfunk
    { unFunk :: ExceptT String (StateT BFContext IO) a
    } deriving
        ( Applicative, Monad, Functor, MonadIO
        , MonadState BFContext)

runProgram :: String -> IO (Either String Output)
runProgram program = do
    let prgZipper = ([], program)
        bufZipper = ([], replicate 1024 0)
    evalStateT (runBrainfunk $ runCode >> gets output) (BFContext prgZipper bufZipper [])

runBrainfunk :: Brainfunk a -> StateT BFContext IO (Either String a)
runBrainfunk funk =
    runExceptT (unFunk funk)

runCode :: Brainfunk ()
runCode = do
    i <- getInstruction
    case i of
      (Just i') -> parseInstruction i' >> nextInstruction >> runCode
      _         -> return ()

throw :: String -> Brainfunk a
throw = Brainfunk . throwE

parseInstruction :: Char -> Brainfunk ()
parseInstruction '>' = nextByte
parseInstruction '<' = prevByte
parseInstruction '+' = incByte
parseInstruction '-' = decByte
parseInstruction '.' = outputByte
parseInstruction ',' = inputByte
parseInstruction '[' = do
    b <- getByte
    when (b == 0) skipLoop
parseInstruction ']' = do
    b <- getByte
    when (b /= 0) repeatLoop
parseInstruction _  = return ()

nextInstruction :: Brainfunk ()
nextInstruction = do
    s <- get
    case forward (program s) of
      (Just p) -> put $ s { program = p }
      _        -> throw "Attempt to get next instruction at end."

prevInstruction :: Brainfunk ()
prevInstruction = do
    prg <- gets program
    case back prg of
      (Just p) -> modify $ \s -> s { program = p }
      _        -> throw "Attempt to go back at beginning of instruction buffer."

getInstruction :: Brainfunk (Maybe Char)
getInstruction = do
    s <- gets program
    case s of
      (_, [])  -> return Nothing
      (_, p:_) -> return $ Just p

repeatLoop :: Brainfunk ()
repeatLoop = do
    i <- getInstruction
    case i of
      (Just '[') -> return ()
      (Just _  ) -> prevInstruction >> repeatLoop
      Nothing    -> throw "Unexpected error."

skipLoop :: Brainfunk ()
skipLoop = do
    i <- getInstruction
    case i of
      (Just ']') -> return ()
      (Just _  ) -> nextInstruction >> skipLoop
      Nothing    -> throw "Unexpected error."

-- nextByte :: Brainfunk ()
nextByte = do
    buf <- gets buffer
    case forward buf of
      (Just b) -> modify $ \s -> s { buffer = b }
      _        -> throw "Attempt to get next byte at end of buffer."

prevByte :: Brainfunk ()
prevByte = do
    buf <- gets buffer
    case back buf of
      (Just b) -> modify $ \s -> s { buffer = b }
      _        -> throw "Attempt to go back at beginning of buffer."

getByte :: Brainfunk Word8
getByte = do
    g <- gets buffer
    case g of
      (_, b:_) -> return b
      _        -> throw "Out of buffer space."

putByte :: Word8 -> Brainfunk ()
putByte b = do
    -- ((t, _:bs), p) <- get
    (t, _:bs) <- gets buffer
    modify $ \s -> s { buffer = (t, b:bs) }

incByte :: Brainfunk ()
incByte =
    getByte >>= putByte . succ

decByte :: Brainfunk ()
decByte =
    getByte >>= putByte . pred

outputByte :: Brainfunk ()
outputByte = do
    b <- getByte
    modify $ \s -> s { output = output s ++ [chr $ fromIntegral b] }

inputByte :: Brainfunk ()
inputByte = throw "Input is not supported."
