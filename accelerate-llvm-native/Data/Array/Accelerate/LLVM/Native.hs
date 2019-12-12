{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements a backend for the /Accelerate/ language targeting
-- multicore CPUs. Expressions are on-line translated into LLVM code, which is
-- just-in-time executed in parallel over the available CPUs. Functions are
-- automatically parallel, provided you specify '+RTS -Nwhatever' on the command
-- line when running the program.
--
-- Programs must be compiled with '-threaded', otherwise you will get a "Blocked
-- indefinitely on MVar" error.
--

module Data.Array.Accelerate.LLVM.Native (

  Acc, Arrays,

  -- * Synchronous execution
  run, runWith,
  run1, run1With,
  runN, runNWith,
  stream, streamWith,
  streamOut, streamOutWith,

  -- * Asynchronous execution
  Async,
  wait, poll, cancel,

  runAsync, runAsyncWith,
  run1Async, run1AsyncWith,
  runNAsync, runNAsyncWith,

  -- * Execution targets
  Native, Strategy,
  createTarget, balancedParIO, unbalancedParIO,

  -- * Ahead-of-time compilation
  runQ,
  runQAsync,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                            ( Arrays )
<<<<<<< HEAD
import Data.Array.Accelerate.AST                                    ( PreOpenAfun(..) )
import Data.Array.Accelerate.Async
import Data.Array.Accelerate.Smart                                  ( Acc )
import Data.Array.Accelerate.Trafo

import Data.Array.Accelerate.LLVM.Execute.Async                     ( AsyncR(..) )
import Data.Array.Accelerate.LLVM.Execute.Environment               ( AvalR(..) )
import Data.Array.Accelerate.LLVM.Native.Array.Data                 ( useRemoteAsync )
import Data.Array.Accelerate.LLVM.Native.Compile                    ( CompiledOpenAfun, compileAcc, compileAfun )
import Data.Array.Accelerate.LLVM.Native.Embed                      ( embedOpenAcc )
import Data.Array.Accelerate.LLVM.Native.Execute                    ( executeAcc, executeOpenAcc )
import Data.Array.Accelerate.LLVM.Native.Execute.Environment        ( Aval )
import Data.Array.Accelerate.LLVM.Native.Link                       ( ExecOpenAfun, linkAcc, linkAfun )
=======
import Data.Array.Accelerate.Smart                                  ( Acc, Seq )
import Data.Array.Accelerate.LLVM.Native.Debug                      as Debug

import Data.Array.Accelerate.LLVM.Native.Compile                    ( compileAcc, compileAfun, compileSeq )
import Data.Array.Accelerate.LLVM.Native.Execute                    ( executeAcc, executeAfun1, executeSeq )
>>>>>>> feature/sequences
import Data.Array.Accelerate.LLVM.Native.State
import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.State                             ( LLVM )
import Data.Array.Accelerate.LLVM.Native.Debug                      as Debug
import qualified Data.Array.Accelerate.LLVM.Native.Execute.Async    as E

-- standard library
import Data.Typeable
import Control.Monad.Trans
import System.IO.Unsafe
import Text.Printf
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH


-- Accelerate: LLVM backend for multicore CPUs
-- -------------------------------------------

-- | Compile and run a complete embedded array program.
--
-- NOTE: it is recommended to use 'run1' whenever possible.
--
run :: Arrays a => Acc a -> a
run = runWith defaultTarget

-- | As 'run', but execute using the specified target (thread gang).
--
runWith :: Arrays a => Native -> Acc a -> a
runWith target a = unsafePerformIO (run' target a)


-- | As 'run', but allow the computation to run asynchronously and return
-- immediately without waiting for the result. The status of the computation can
-- be queried using 'wait', 'poll', and 'cancel'.
--
runAsync :: Arrays a => Acc a -> IO (Async a)
runAsync = runAsyncWith defaultTarget

-- | As 'runAsync', but execute using the specified target (thread gang).
--
runAsyncWith :: Arrays a => Native -> Acc a -> IO (Async a)
runAsyncWith target a = async (run' target a)

run' :: Arrays a => Native -> Acc a -> IO a
run' target a = execute
  where
    !acc        = convertAccWith (config target) a
    execute     = do
      dumpGraph acc
      evalNative target $ do
        build <- phase "compile" elapsedS (compileAcc acc) >>= dumpStats
        exec  <- phase "link"    elapsedS (linkAcc build)
        res   <- phase "execute" elapsedP (executeAcc exec)
        return res


-- | This is 'runN', specialised to an array program of one argument.
--
run1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b
run1 = run1With defaultTarget

-- | As 'run1', but execute using the specified target (thread gang).
--
run1With :: (Arrays a, Arrays b) => Native -> (Acc a -> Acc b) -> a -> b
run1With = runNWith


-- | Prepare and execute an embedded array program.
--
-- This function can be used to improve performance in cases where the array
-- program is constant between invocations, because it enables us to bypass
-- front-end conversion stages and move directly to the execution phase. If you
-- have a computation applied repeatedly to different input data, use this,
-- specifying any changing aspects of the computation via the input parameters.
-- If the function is only evaluated once, this is equivalent to 'run'.
--
-- In order to use 'runN' you must express your Accelerate program as a function
-- of array terms:
--
-- > f :: (Arrays a, Arrays b, ... Arrays c) => Acc a -> Acc b -> ... -> Acc c
--
-- This function then returns the compiled version of 'f':
--
-- > runN f :: (Arrays a, Arrays b, ... Arrays c) => a -> b -> ... -> c
--
-- At an example, rather than:
--
-- > step :: Acc (Vector a) -> Acc (Vector b)
-- > step = ...
-- >
-- > simulate :: Vector a -> Vector b
-- > simulate xs = run $ step (use xs)
--
-- Instead write:
--
-- > simulate = runN step
--
-- You can use the debugging options to check whether this is working
-- successfully. For example, running with the @-ddump-phases@ flag should show
-- that the compilation steps only happen once, not on the second and subsequent
-- invocations of 'simulate'. Note that this typically relies on GHC knowing
-- that it can lift out the function returned by 'runN' and reuse it.
--
-- See the programs in the 'accelerate-examples' package for examples.
--
runN :: Afunction f => f -> AfunctionR f
runN = runNWith defaultTarget

-- | As 'runN', but execute using the specified target (thread gang).
--
runNWith :: Afunction f => Native -> f -> AfunctionR f
runNWith target f = exec
  where
    !acc  = convertAfunWith (config target) f
    !afun = unsafePerformIO $ do
              dumpGraph acc
              evalNative target $ do
                build <- phase "compile" elapsedS (compileAfun acc) >>= dumpStats
                link  <- phase "link"    elapsedS (linkAfun build)
                return link
    !exec = go afun (return Aempty)

    go :: ExecOpenAfun Native aenv t -> LLVM Native (Aval aenv) -> t
    go (Alam l) k = \arrs ->
      let k' = do aenv       <- k
                  AsyncR _ a <- E.async (useRemoteAsync arrs)
                  return (aenv `Apush` a)
      in go l k'
    go (Abody b) k = unsafePerformIO . phase "execute" elapsedP . evalNative target $ do
      aenv   <- k
      E.get =<< E.async (executeOpenAcc b aenv)


-- | As 'run1', but execute asynchronously.
--
run1Async :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> IO (Async b)
run1Async = run1AsyncWith defaultTarget

-- | As 'run1Async', but execute using the specified target (thread gang).
--
run1AsyncWith :: (Arrays a, Arrays b) => Native -> (Acc a -> Acc b) -> a -> IO (Async b)
run1AsyncWith = runNAsyncWith


-- | As 'runN', but execute asynchronously.
--
runNAsync :: (Afunction f, RunAsync r, AfunctionR f ~ RunAsyncR r) => f -> r
runNAsync = runNAsyncWith defaultTarget

-- | As 'runNWith', but execute asynchronously.
--
runNAsyncWith :: (Afunction f, RunAsync r, AfunctionR f ~ RunAsyncR r) => Native -> f -> r
runNAsyncWith target f = runAsync' target afun (return Aempty)
  where
    !acc  = convertAfunWith (config target) f
    !afun = unsafePerformIO $ do
              dumpGraph acc
              evalNative target $ do
                build <- phase "compile" elapsedS (compileAfun acc) >>= dumpStats
                link  <- phase "link"    elapsedS (linkAfun build)
                return link

class RunAsync f where
  type RunAsyncR f
  runAsync' :: Native -> ExecOpenAfun Native aenv (RunAsyncR f) -> LLVM Native (Aval aenv) -> f

instance RunAsync b => RunAsync (a -> b) where
  type RunAsyncR (a -> b) = a -> RunAsyncR b
  runAsync' _      Abody{}  _ _    = error "runAsync: function oversaturated"
  runAsync' target (Alam l) k arrs =
    let k' = do aenv       <- k
                AsyncR _ a <- E.async (useRemoteAsync arrs)
                return (aenv `Apush` a)
    in runAsync' target l k'

instance RunAsync (IO (Async b)) where
  type RunAsyncR  (IO (Async b)) = b
  runAsync' _      Alam{}    _ = error "runAsync: function not fully applied"
  runAsync' target (Abody b) k = async . phase "execute" elapsedP . evalNative target $ do
    aenv   <- k
    E.get =<< E.async (executeOpenAcc b aenv)


-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go.
--
stream :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> [a] -> [b]
stream = streamWith defaultTarget

-- | As 'stream', but execute using the specified target (thread gang).
--
streamWith :: (Arrays a, Arrays b) => Native -> (Acc a -> Acc b) -> [a] -> [b]
streamWith target f arrs = map go arrs
  where
    !go = run1With target f

streamOut :: Arrays a => Seq [a] -> [a]
streamOut = streamOutWith defaultTarget

streamOutWith :: Arrays a => Native -> Seq [a] -> [a]
streamOutWith target s =
  let
    s'  = convertSeqWith (config target) s
    s'' = unsafePerformIO $ phase "compile" elapsedS (evalNative target (compileSeq s')) >>= dumpStats
  in unsafePerformIO $ phase "execute" elapsedP (evalNative target (executeSeq s''))


-- | Ahead-of-time compilation for an embedded array program.
--
-- This function will generate, compile, and link into the final executable,
-- code to execute the given Accelerate computation /at Haskell compile time/.
-- This eliminates any runtime overhead associated with the other @run*@
-- operations. The generated code will be optimised for the compiling
-- architecture.
--
-- Since the Accelerate program will be generated at Haskell compile time,
-- construction of the Accelerate program, in particular via meta-programming,
-- will be limited to operations available to that phase. Also note that any
-- arrays which are embedded into the program via 'Data.Array.Accelerate.use'
-- will be stored as part of the final executable.
--
-- In order to link the final program together, the
-- <http://hackage.haskell.org/package/accelerate-llvm-plugin accelerate-llvm-plugin>
-- GHC plugin must be run over any modules using this function, for example by
-- adding the following pragma to the top of the file:
--
-- > {-# OPTIONS_GHC -fplugin=Data.Array.Accelerate.LLVM.Plugin #-}
--
-- To load the file in interactive mode (ghci), you will also need to specify
-- the option:
--
-- > -fplugin-opt=Data.Array.Accelerate.LLVM.Plugin:interactive
--
-- The wrapper script @ghc-accelerate@ (part of the @accelerate-llvm-plugin@
-- package) can be used in place of the regular @ghc@ program, and will insert
-- the above options for you as necessary.
--
-- [/Note:/]
--
-- Due to <https://ghc.haskell.org/trac/ghc/ticket/13587 GHC#13587>, this
-- currently must be as an /untyped/ splice.
--
-- The correct type of this function is similar to that of 'runN':
--
-- > runQ :: Afunction f => f -> Q (TExp (AfunctionR f))
--
-- Since 1.1.0.0. Requires GHC-8.0.
--
runQ :: Afunction f => f -> TH.ExpQ
runQ = runQ' [| unsafePerformIO |]


-- | Ahead-of-time analogue of 'runNAsync'. See 'runQ' for more information.
--
-- The correct type of this function is:
--
-- > runQAsync :: (Afunction f, RunAsync r, AfunctionR f ~ RunAsyncR r) => f -> Q (TExp r)
--
-- Since 1.1.0.0. Requires GHC-8.0.
--
runQAsync :: Afunction f => f -> TH.ExpQ
runQAsync = runQ' [| async |]


runQ' :: Afunction f => TH.ExpQ -> f -> TH.ExpQ
runQ' using f = do
  -- XXX: The reification of segmented operations (in particular, foldSeg) will
  --      depend on the gang size at _compile_ time.
  --
  afun  <- let acc = convertAfunWith (config defaultTarget) f
           in  TH.runIO $ do
                 dumpGraph acc
                 evalNative defaultTarget $
                   phase "compile" elapsedS (compileAfun acc) >>= dumpStats

  -- generate a lambda function with the correct number of arguments and apply
  -- directly to the body expression.
  let
      go :: Typeable aenv => CompiledOpenAfun Native aenv t -> [TH.PatQ] -> [TH.ExpQ] -> [TH.StmtQ] -> TH.ExpQ
      go (Alam lam) xs as stmts = do
        x <- TH.newName "x" -- lambda bound variable
        a <- TH.newName "a" -- local array name
        s <- TH.bindS (TH.conP 'AsyncR [TH.wildP, TH.varP a]) [| E.async (useRemoteAsync $(TH.varE x)) |]
        go lam (TH.varP x : xs) (TH.varE a : as) (return s : stmts)

      go (Abody body) xs as stmts =
        let aenv = foldr (\a gamma -> [| $gamma `Apush` $a |] ) [| Aempty |] as
            eval = TH.noBindS [| E.get =<< E.async (executeOpenAcc $(TH.unTypeQ (embedOpenAcc defaultTarget body)) $aenv) |]
        in
        TH.lamE (reverse xs) [| $using . phase "execute" elapsedP . evalNative defaultTarget $
                                  $(TH.doE (reverse (eval : stmts))) |]
  --
  go afun [] [] []


-- How the Accelerate program should be evaluated.
--
-- TODO: make sharing/fusion runtime configurable via debug flags or otherwise.
--
config :: Native -> Phase
config target = phases
  { convertOffsetOfSegment = gangSize target > 1
  , convertSubarrayToIndex = True
  }


-- Debugging
-- =========

dumpStats :: MonadIO m => a -> m a
dumpStats x = dumpSimplStats >> return x

phase :: MonadIO m => String -> (Double -> Double -> String) -> m a -> m a
phase n fmt go = timed dump_phases (\wall cpu -> printf "phase %s: %s" n (fmt wall cpu)) go

