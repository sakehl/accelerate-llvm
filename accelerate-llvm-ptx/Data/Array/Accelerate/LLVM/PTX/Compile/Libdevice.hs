{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice (

  withLibdeviceNVVM,
  withLibdeviceNVPTX,

) where

-- llvm-hs
import LLVM.Context
import qualified LLVM.Module                                        as LLVM

import LLVM.AST                                                     as AST
import LLVM.AST.Global                                              as G
import LLVM.AST.Linkage

-- accelerate
import Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.Load
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

-- cuda
import Foreign.CUDA.Analysis
import Foreign.CUDA.Path
import qualified Foreign.CUDA.Driver                                as CUDA

-- standard library
import Control.Monad
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short.Char8                                  ( ShortByteString )
import Data.HashSet                                                 ( HashSet )
import Data.List
import Data.Maybe
import Text.Printf
<<<<<<< HEAD
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Data.ByteString.Short.Extra                        as BS
import qualified Data.HashSet                                       as Set
=======
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Char8                              as B8
import qualified Data.HashMap.Strict                                as HashMap
import Data.ByteString.Short                                        ( ShortByteString )


-- NVVM Reflect
-- ------------

class NVVMReflect a where
  nvvmReflect :: a
>>>>>>> feature/sequences


-- | Lower an LLVM AST to C++ objects and link it against the libdevice module,
-- iff any libdevice functions are referenced from the base module.
--
-- Note: [Linking with libdevice]
--
-- The CUDA toolkit comes with an LLVM bitcode library called 'libdevice' that
-- implements many common mathematical functions. The library can be used as a
-- high performance math library for targets of the LLVM NVPTX backend, such as
-- this one. To link a module 'foo' with libdevice, the following compilation
-- pipeline is recommended:
--
--   1. Save all external functions in module 'foo'
--
--   2. Link module 'foo' with the appropriate 'libdevice_compute_XX.YY.bc'
--
<<<<<<< HEAD
--   3. Internalise all functions not in the list from (1)
=======
nvvmReflectPass_mdl :: AST.Module
nvvmReflectPass_mdl =
  AST.Module
    { moduleName            = "nvvm-reflect"
    , moduleSourceFileName  = mempty
    , moduleDataLayout      = targetDataLayout (undefined::PTX)
    , moduleTargetTriple    = targetTriple (undefined::PTX)
    , moduleDefinitions     = [GlobalDefinition $ functionDefaults
      { name                  = AST.Name "__nvvm_reflect"
      , returnType            = downcast (integralType :: IntegralType Int32)
      , parameters            = ( [ptrParameter scalarType (UnName 0 :: Name (Ptr Int8))], False )
      , G.functionAttributes  = map Right [NoUnwind, ReadNone, AlwaysInline]
      , basicBlocks           = []
      }]
    }

{-# NOINLINE nvvmReflectPass_bc #-}
nvvmReflectPass_bc :: (String, ByteString)
nvvmReflectPass_bc = (name,) . unsafePerformIO $ do
  withContext $ \ctx -> do
    withModuleFromAST ctx nvvmReflectPass_mdl (return <=< moduleLLVMAssembly)
  where
    name     = "__nvvm_reflect"
    --runError = either ($internalError "nvvmReflectPass") return <=< runExceptT


-- libdevice
-- ---------

-- Compatible version of libdevice for a given compute capability should belibdevice ::
-- listed here:libdevice ::
>>>>>>> feature/sequences
--
--   4. Eliminate all unused internal functions
--
<<<<<<< HEAD
--   5. Run the NVVMReflect pass (see note: [NVVM Reflect Pass])
=======
class Libdevice a where
  libdevice :: Compute -> a

instance Libdevice AST.Module where
  libdevice _
    | CUDA.libraryVersion >= 9000
    = libdevice_50_mdl
  --
  libdevice (Compute n m) =
    case (n,m) of
      (2,_)             -> libdevice_20_mdl   -- 2.0, 2.1
      (3,x) | x < 5     -> libdevice_30_mdl   -- 3.0, 3.2
            | otherwise -> libdevice_35_mdl   -- 3.5, 3.7
      (5,_)             -> libdevice_50_mdl   -- 5.x
      (6,_)             -> libdevice_50_mdl   -- 6.x
      _                 -> $internalError "libdevice" "no binary for this architecture"

instance Libdevice (String, ByteString) where
  libdevice _
    | CUDA.libraryVersion >= 9000
    = libdevice_50_bc
  --
  libdevice (Compute n m) =
    case (n,m) of
      (2,_)             -> libdevice_20_bc    -- 2.0, 2.1
      (3,x) | x < 5     -> libdevice_30_bc    -- 3.0, 3.2
            | otherwise -> libdevice_35_bc    -- 3.5, 3.7
      (5,_)             -> libdevice_50_bc    -- 5.x
      (6,_)             -> libdevice_50_bc    -- 6.x
      _                 -> $internalError "libdevice" "no binary for this architecture"


-- Load the libdevice bitcode files as an LLVM AST module. The top-level
-- unsafePerformIO ensures that the data is only read from disk once per program
-- execution.
>>>>>>> feature/sequences
--
--   6. Run the standard optimisation pipeline
--
<<<<<<< HEAD
withLibdeviceNVPTX
    :: DeviceProperties
    -> Context
    -> Module
    -> (LLVM.Module -> IO a)
    -> IO a
withLibdeviceNVPTX dev ctx ast next =
  case Set.null externs of
    True        -> LLVM.withModuleFromAST ctx ast next
    False       ->
      LLVM.withModuleFromAST ctx ast                          $ \mdl  ->
      LLVM.withModuleFromAST ctx nvvmReflect                  $ \refl ->
      LLVM.withModuleFromAST ctx (internalise externs libdev) $ \libd -> do
        LLVM.linkModules mdl refl
        LLVM.linkModules mdl libd
        Debug.traceIO Debug.dump_cc msg
        next mdl
  where
    -- Replace the target triple and datalayout from the libdevice.bc module
    -- with those of the generated code. This avoids warnings such as "linking
    -- two modules of different target triples..."
    libdev      = (libdevice arch) { moduleTargetTriple = moduleTargetTriple ast
                                   , moduleDataLayout   = moduleDataLayout ast
                                   }
    externs     = analyse ast
    arch        = computeCapability dev

    msg         = printf "cc: linking with libdevice: %s"
                $ intercalate ", "
                $ map S8.unpack
                $ Set.toList externs


-- | Lower an LLVM AST to C++ objects and prepare it for linking against
-- libdevice using the nvvm bindings, iff any libdevice functions are referenced
-- from the base module.
--
-- Rather than internalise and strip any unused functions ourselves, allow the
-- nvvm library to do so when linking the two modules together.
--
-- TLM: This really should work with the above method, however for some reason
-- we get a "CUDA Exception: function named symbol not found" error, even though
-- the function is clearly visible in the generated code. hmm...
--
withLibdeviceNVVM
    :: DeviceProperties
    -> Context
    -> Module
    -> ([(String, ByteString)] -> LLVM.Module -> IO a)
    -> IO a
withLibdeviceNVVM dev ctx ast next =
  LLVM.withModuleFromAST ctx ast $ \mdl -> do
    when withlib $ Debug.traceIO Debug.dump_cc msg
    next lib mdl
  where
    externs             = analyse ast
    withlib             = not (Set.null externs)
    lib | withlib       = [ nvvmReflect, libdevice arch ]
        | otherwise     = []

    arch        = computeCapability dev

    msg         = printf "cc: linking with libdevice: %s"
                $ intercalate ", "
                $ map S8.unpack
                $ Set.toList externs


-- | Analyse the LLVM AST module and determine if any of the external
-- declarations are intrinsics implemented by libdevice. The set of such
-- functions is returned, and will be used when determining which functions from
-- libdevice to internalise.
--
analyse :: Module -> HashSet ShortByteString
analyse Module{..} =
  let intrinsic (GlobalDefinition Function{..})
        | null basicBlocks
        , Name n        <- name
        , "__nv_"       <- BS.take 5 n
        = Just n

      intrinsic _
        = Nothing
  in
  Set.fromList (mapMaybe intrinsic moduleDefinitions)
=======
{-# NOINLINE libdevice_20_bc #-}
{-# NOINLINE libdevice_30_bc #-}
{-# NOINLINE libdevice_35_bc #-}
{-# NOINLINE libdevice_50_bc #-}
libdevice_20_bc, libdevice_30_bc, libdevice_35_bc, libdevice_50_bc :: (String,ByteString)
libdevice_20_bc = unsafePerformIO $ libdeviceBitcode (Compute 2 0)
libdevice_30_bc = unsafePerformIO $ libdeviceBitcode (Compute 3 0)
libdevice_35_bc = unsafePerformIO $ libdeviceBitcode (Compute 3 5)
libdevice_50_bc = unsafePerformIO $ libdeviceBitcode (Compute 5 0)


-- Load the libdevice bitcode file for the given compute architecture, and raise
-- it to a Haskell AST that can be kept for future use. The name of the bitcode
-- files follows:
--
--   libdevice.compute_XX.YY.bc
--
-- Where XX represents the compute capability, and YY represents a version(?) We
-- search the libdevice PATH for all files of the appropriate compute capability
-- and load the most recent.
--
libdeviceModule :: Compute -> IO AST.Module
libdeviceModule arch = do
  let bc :: (String, ByteString)
      bc = libdevice arch

  -- TLM: we have called 'withContext' again here, although the LLVM state
  --      already carries a version of the context. We do this so that we can
  --      fully apply this function that can be lifted out to a CAF and only
  --      executed once per program execution.
  --
  withContext $ \ctx ->
    withModuleFromBitcode ctx bc moduleAST


-- Load the libdevice bitcode file for the given compute architecture. The name
-- of the bitcode files follows the format:
--
--   libdevice.compute_XX.YY.bc
--
-- Where XX represents the compute capability, and YY represents a version(?) We
-- search the libdevice PATH for all files of the appropriate compute capability
-- and load the "most recent" (by sort order).
--
libdeviceBitcode :: Compute -> IO (String, ByteString)
libdeviceBitcode (Compute m n) = do
  let arch       | CUDA.libraryVersion < 9000 = printf "libdevice.compute_%d%d" m n
                 | otherwise                  = "libdevice"
      err        = $internalError "libdevice" (printf "not found: %s.YY.bc" arch)
      best f     = arch `isPrefixOf` f && takeExtension f == ".bc"
      base       = cudaInstallPath </> "nvvm" </> "libdevice"

  files <- getDirectoryContents base
  name  <- maybe err return . listToMaybe . sortBy (flip compare) $ filter best files
  bc    <- B.readFile (base </> name)

  return (name, bc)
>>>>>>> feature/sequences


-- | Mark all definitions in the module as internal linkage. This means that
-- unused definitions can be removed as dead code. Be careful to leave any
-- declarations as external.
--
<<<<<<< HEAD
internalise :: HashSet ShortByteString -> Module -> Module
internalise externals Module{..} =
  let internal (GlobalDefinition Function{..})
        | Name n <- name
        , not (Set.member n externals)          -- we don't call this function directly; and
        , not (null basicBlocks)                -- it is not an external declaration
        = GlobalDefinition Function { linkage=Internal, .. }

      internal x
        = x
=======
-- libdevicePath :: IO FilePath
-- libdevicePath = do
--   nvcc  <- fromMaybe (error "could not find 'nvcc' in PATH") `fmap` findExecutable "nvcc"

--   let ccvn = reverse (splitPath nvcc)
--       dir  = "libdevice" : "nvvm" : drop 2 ccvn

--   return (joinPath (reverse dir))


instance Intrinsic PTX where
  intrinsicForTarget _ = libdeviceIndex

-- The list of functions implemented by libdevice. These are all more-or-less
-- named consistently based on the standard mathematical functions they
-- implement, with the "__nv_" prefix stripped.
--
libdeviceIndex :: HashMap ShortByteString Label
libdeviceIndex =
  let nv base   = (base, Label $ "__nv_" <> base)
>>>>>>> feature/sequences
  in
  Module { moduleDefinitions = map internal moduleDefinitions, .. }

