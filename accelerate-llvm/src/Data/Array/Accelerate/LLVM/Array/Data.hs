{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Array.Data
-- Copyright   : [2014..2017] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Array.Data (

  Remote(..),
  newRemote,
  useRemote,    useRemoteAsync,
  copyToRemote, copyToRemoteAsync,
  copyToHost,   copyToHostAsync,
  copyToPeer,   copyToPeerAsync,

  runIndexArray,
  runArrays,
  runArray,

  module Data.Array.Accelerate.Array.Data,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Execute.Async

-- standard library
import Control.Monad                                                ( liftM, liftM2 )
import Control.Monad.Trans
import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Prelude


class Async arch => Remote arch where

  -- | Allocate a new uninitialised array on the remote device.
  --
  {-# INLINEABLE allocateRemote #-}
  allocateRemote :: (Shape sh, Elt e) => sh -> LLVM arch (Array sh e)
  allocateRemote sh = liftIO $ allocateArray sh

  -- | Use the given immutable array on the remote device. Since the source
  -- array is immutable, the allocator can evict and re-upload the data as
  -- necessary without copy-back.
  --
  {-# INLINEABLE useRemoteR #-}
  useRemoteR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ number of elements to copy
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ array payload
      -> LLVM arch ()
  useRemoteR _ _ _ = return ()

  -- | Upload a section of an array from the host to the remote device.
  --
  {-# INLINEABLE copyToRemoteR #-}
  copyToRemoteR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ index of first element to copy
      -> Int                      -- ^ number of elements to copy
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ array payload
      -> LLVM arch ()
  copyToRemoteR _ _ _ _ = return ()

  -- | Copy a section of an array from the remote device back to the host.
  --
  {-# INLINEABLE copyToHostR #-}
  copyToHostR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ index of the first element to copy
      -> Int                      -- ^ number of elements to copy
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ array payload
      -> LLVM arch ()
  copyToHostR _ _ _ _ = return ()

  -- | Copy a section of an array between two remote instances of the same type.
  -- This may be more efficient than copying to the host and then to the second
  -- remote instance (e.g. DMA between two CUDA devices). The elements between
  -- the given indices (inclusive left, exclusive right) are transferred.
  --
  {-# INLINEABLE copyToPeerR #-}
  copyToPeerR
      :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e)
      => Int                      -- ^ index of the first element to copy
      -> Int                      -- ^ number of elements to copy
      -> arch                     -- ^ remote device to copy to
      -> Maybe (StreamR arch)     -- ^ execute synchronously w.r.t. this execution stream
      -> ArrayData e              -- ^ array payload
      -> LLVM arch ()
  copyToPeerR _ _ _ _ _ = return ()

  -- | Read a single element from the array at a given row-major index
  --
  {-# INLINEABLE indexRemote #-}
  indexRemote :: Array sh e -> Int -> LLVM arch e
  indexRemote (Array _ adata) i = return . toElt $! unsafeIndexArrayData adata i


-- | Create a new array from its representation on the host, and upload it to
-- a new remote array.
--
{-# INLINEABLE newRemote #-}
newRemote
    :: (Remote arch, Shape sh, Elt e)
    => sh
    -> (sh -> e)
    -> LLVM arch (Array sh e)
newRemote sh f =
  useRemote $! fromFunction sh f


-- | Upload an immutable array from the host to the remote device. This is
-- a synchronous operation in that it will not return until the transfer
-- completes, but the individual array payloads will be uploaded concurrently if
-- possible.
--
{-# INLINEABLE useRemote #-}
useRemote :: (Remote arch, Arrays arrs) => arrs -> LLVM arch arrs
useRemote arrs = do
  AsyncR _ a <- async (useRemoteAsync arrs)
  get a


-- | Upload an immutable array from the host to the remote device,
-- asynchronously. This will upload each array payload in a separate execution
-- stream, thereby making us of multiple memcpy engines (where available).
--
{-# INLINEABLE useRemoteAsync #-}
useRemoteAsync
    :: (Remote arch, Arrays arrs)
    => arrs
    -> StreamR arch
    -> LLVM arch (AsyncR arch arrs)
useRemoteAsync arrs stream = do
  arrs' <- runArrays arrs $ \arr@Array{} ->
    let n = size (shape arr)
    in  runArray arr $ \scale ad -> do
          s <- fork
          useRemoteR (scale n) (Just s) ad
          after stream =<< checkpoint s
          join s
          return ad
  --
  event  <- checkpoint stream   -- TLM: Assuming that adding events to a stream counts as things to wait for
  return $! AsyncR event arrs'


-- | Uploading existing arrays from the host to the remote device. This is
-- synchronous with respect to the calling thread, but the individual array
-- payloads may themselves be transferred concurrently.
--
{-# INLINEABLE copyToRemote #-}
copyToRemote :: (Remote arch, Arrays a) => a -> LLVM arch a
copyToRemote arrs = do
  AsyncR _ a <- async (copyToRemoteAsync arrs)
  get a


-- | Upload an existing array to the remote device, asynchronously.
--
{-# INLINEABLE copyToRemoteAsync #-}
copyToRemoteAsync
    :: (Remote arch, Arrays a)
    => a
    -> StreamR arch
    -> LLVM arch (AsyncR arch a)
copyToRemoteAsync arrs stream = do
  arrs' <- runArrays arrs $ \arr@Array{} ->
    let n = size (shape arr)
    in  runArray arr $ \scale ad -> do
          s <- fork
          copyToRemoteR 0 (scale n) (Just s) ad
          after stream =<< checkpoint s
          join s
          return ad
  --
  event  <- checkpoint stream
  return $! AsyncR event arrs'


-- | Copy an array from the remote device to the host. This is synchronous with
-- respect to the calling thread, but the individual array payloads may
-- themselves be transferred concurrently.
--
{-# INLINEABLE copyToHost #-}
copyToHost :: (Remote arch, Arrays a) => a -> LLVM arch a
copyToHost arrs = do
  AsyncR _ a <- async (copyToHostAsync arrs)
  get a


-- | Copy an array from the remote device to the host, asynchronously
--
{-# INLINEABLE copyToHostAsync #-}
copyToHostAsync
    :: (Remote arch, Arrays a)
    => a
    -> StreamR arch
    -> LLVM arch (AsyncR arch a)
copyToHostAsync arrs stream = do
  arrs' <- runArrays arrs $ \arr@Array{} ->
    let n = size (shape arr)
    in  runArray arr $ \scale ad -> do
          s <- fork
          copyToHostR 0 (scale n) (Just s) ad
          after stream =<< checkpoint s
          join s
          return ad
  --
  event  <- checkpoint stream
  return $! AsyncR event arrs'


-- | Copy arrays between two remote instances of the same type. This may be more
-- efficient than copying to the host and then to the second remote instance
-- (e.g. DMA between CUDA devices).
--
{-# INLINEABLE copyToPeer #-}
copyToPeer :: (Remote arch, Arrays a) => arch -> a -> LLVM arch a
copyToPeer peer arrs = do
  AsyncR _ a <- async (copyToPeerAsync peer arrs)
  get a


-- | As 'copyToPeer', asynchronously.
--
{-# INLINEABLE copyToPeerAsync #-}
copyToPeerAsync
    :: (Remote arch, Arrays a)
    => arch
    -> a
    -> StreamR arch
    -> LLVM arch (AsyncR arch a)
copyToPeerAsync peer arrs stream = do
  arrs' <- runArrays arrs $ \arr@Array{} ->
    let n = size (shape arr)
    in  runArray arr $ \scale ad -> do
          s <- fork
          copyToPeerR 0 (scale n) peer (Just s) ad
          after stream =<< checkpoint s
          join s
          return ad
  --
  event  <- checkpoint stream
  return $! AsyncR event arrs'


-- Helpers for traversing the Arrays data structure
-- ------------------------------------------------

-- |Read a single element from an array at the given row-major index.
--
{-# INLINEABLE runIndexArray #-}
runIndexArray
    :: forall m sh e. Monad m
    => (forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, Storable a, Typeable a, Typeable e) => ArrayData e -> Int -> m a)
    -> Array sh e
    -> Int
    -> m e
runIndexArray worker (Array _ adata) ix = toElt `liftM` indexR arrayElt adata ix
  where
    indexR :: ArrayEltR a -> ArrayData a -> Int -> m a
    indexR ArrayEltRunit    _  _ = return ()
    indexR ArrayEltRint     ad i = worker ad i
    indexR ArrayEltRint8    ad i = worker ad i
    indexR ArrayEltRint16   ad i = worker ad i
    indexR ArrayEltRint32   ad i = worker ad i
    indexR ArrayEltRint64   ad i = worker ad i
    indexR ArrayEltRword    ad i = worker ad i
    indexR ArrayEltRword8   ad i = worker ad i
    indexR ArrayEltRword16  ad i = worker ad i
    indexR ArrayEltRword32  ad i = worker ad i
    indexR ArrayEltRword64  ad i = worker ad i
    indexR ArrayEltRhalf    ad i = worker ad i
    indexR ArrayEltRfloat   ad i = worker ad i
    indexR ArrayEltRdouble  ad i = worker ad i
    indexR ArrayEltRchar    ad i = worker ad i
    indexR ArrayEltRcshort  ad i = CShort  `liftM` worker ad i
    indexR ArrayEltRcushort ad i = CUShort `liftM` worker ad i
    indexR ArrayEltRcint    ad i = CInt    `liftM` worker ad i
    indexR ArrayEltRcuint   ad i = CUInt   `liftM` worker ad i
    indexR ArrayEltRclong   ad i = CLong   `liftM` worker ad i
    indexR ArrayEltRculong  ad i = CULong  `liftM` worker ad i
    indexR ArrayEltRcllong  ad i = CLLong  `liftM` worker ad i
    indexR ArrayEltRcullong ad i = CULLong `liftM` worker ad i
    indexR ArrayEltRcchar   ad i = CChar   `liftM` worker ad i
    indexR ArrayEltRcschar  ad i = CSChar  `liftM` worker ad i
    indexR ArrayEltRcuchar  ad i = CUChar  `liftM` worker ad i
    indexR ArrayEltRcfloat  ad i = CFloat  `liftM` worker ad i
    indexR ArrayEltRcdouble ad i = CDouble `liftM` worker ad i
    indexR ArrayEltRbool    ad i = toBool  `liftM` worker ad i
      where
        toBool 0 = False
        toBool _ = True
    --
    indexR (ArrayEltRpair aeR1 aeR2) (AD_Pair ad1 ad2) i =
      (,) <$> indexR aeR1 ad1 i
          <*> indexR aeR2 ad2 i

    -- NOTE [indexArray for SIMD vector types]
    --
    -- These data types are stored contiguously in memory. Especially for
    -- backends where data is stored in a separate memory space (i.e. GPUs) we
    -- should copy all of those values in a single transaction, before unpacking
    -- them to the appropriate Haskell value (and/or, also store these values
    -- contiguously in Haskell land). @speed
    --
    indexR (ArrayEltRvec2 r) (AD_V2 ad) i =
      let i' = 2*i
      in  V2 <$> indexR r ad i'
             <*> indexR r ad (i'+1)

    indexR (ArrayEltRvec3 r) (AD_V3 ad) i =
      let i' = 3*i
      in  V3 <$> indexR r ad i'
             <*> indexR r ad (i'+1)
             <*> indexR r ad (i'+2)

    indexR (ArrayEltRvec4 r) (AD_V4 ad) i =
      let i' = 4*i
      in  V4 <$> indexR r ad i'
             <*> indexR r ad (i'+1)
             <*> indexR r ad (i'+2)
             <*> indexR r ad (i'+3)

    indexR (ArrayEltRvec8 r) (AD_V8 ad) i =
      let i' = 8*i
      in  V8 <$> indexR r ad i'
             <*> indexR r ad (i'+1)
             <*> indexR r ad (i'+2)
             <*> indexR r ad (i'+3)
             <*> indexR r ad (i'+4)
             <*> indexR r ad (i'+5)
             <*> indexR r ad (i'+6)
             <*> indexR r ad (i'+7)

    indexR (ArrayEltRvec16 r) (AD_V16 ad) i =
      let i' = 16*i
      in  V16 <$> indexR r ad i'
              <*> indexR r ad (i'+1)
              <*> indexR r ad (i'+2)
              <*> indexR r ad (i'+3)
              <*> indexR r ad (i'+4)
              <*> indexR r ad (i'+5)
              <*> indexR r ad (i'+6)
              <*> indexR r ad (i'+7)
              <*> indexR r ad (i'+8)
              <*> indexR r ad (i'+9)
              <*> indexR r ad (i'+10)
              <*> indexR r ad (i'+11)
              <*> indexR r ad (i'+12)
              <*> indexR r ad (i'+13)
              <*> indexR r ad (i'+14)
              <*> indexR r ad (i'+15)


-- | Generalised function to traverse the Arrays structure
--
{-# INLINE runArrays #-}
runArrays
    :: forall m arrs. (Monad m, Arrays arrs)
    => arrs
    -> (forall sh e. Array sh e -> m (Array sh e))
    -> m arrs
runArrays arrs worker = toArr `liftM` runR (arrays arrs) (fromArr arrs)
  where
    runR :: ArraysR a -> a -> m a
    runR ArraysRunit             ()             = return ()
    runR ArraysRarray            arr            = worker arr
    runR (ArraysRpair aeR2 aeR1) (arrs2, arrs1) = liftM2 (,) (runR aeR2 arrs2) (runR aeR1 arrs1)


-- | Generalised function to traverse the ArrayData structure with one
-- additional argument
--
{-# INLINE runArray #-}
runArray
    :: forall m sh e. Monad m
    => Array sh e
    -> (forall e' p. (ArrayElt e', ArrayPtrs e' ~ Ptr p, Storable p, Typeable p, Typeable e') => (Int -> Int) -> ArrayData e' -> m (ArrayData e'))
    -> m (Array sh e)
runArray (Array sh adata) worker = Array sh `liftM` runR arrayElt adata id
  where
    runR :: ArrayEltR e' -> ArrayData e' -> (Int -> Int) -> m (ArrayData e')
    runR ArrayEltRint              ad                k = worker k ad
    runR ArrayEltRint8             ad                k = worker k ad
    runR ArrayEltRint16            ad                k = worker k ad
    runR ArrayEltRint32            ad                k = worker k ad
    runR ArrayEltRint64            ad                k = worker k ad
    runR ArrayEltRword             ad                k = worker k ad
    runR ArrayEltRword8            ad                k = worker k ad
    runR ArrayEltRword16           ad                k = worker k ad
    runR ArrayEltRword32           ad                k = worker k ad
    runR ArrayEltRword64           ad                k = worker k ad
    runR ArrayEltRhalf             ad                k = worker k ad
    runR ArrayEltRfloat            ad                k = worker k ad
    runR ArrayEltRdouble           ad                k = worker k ad
    runR ArrayEltRbool             ad                k = worker k ad
    runR ArrayEltRchar             ad                k = worker k ad
    runR ArrayEltRcshort           ad                k = worker k ad
    runR ArrayEltRcushort          ad                k = worker k ad
    runR ArrayEltRcint             ad                k = worker k ad
    runR ArrayEltRcuint            ad                k = worker k ad
    runR ArrayEltRclong            ad                k = worker k ad
    runR ArrayEltRculong           ad                k = worker k ad
    runR ArrayEltRcllong           ad                k = worker k ad
    runR ArrayEltRcullong          ad                k = worker k ad
    runR ArrayEltRcfloat           ad                k = worker k ad
    runR ArrayEltRcdouble          ad                k = worker k ad
    runR ArrayEltRcchar            ad                k = worker k ad
    runR ArrayEltRcschar           ad                k = worker k ad
    runR ArrayEltRcuchar           ad                k = worker k ad
    runR ArrayEltRunit             AD_Unit           _ = return AD_Unit
    runR (ArrayEltRvec2 ae)        (AD_V2 ad)        k = liftM  AD_V2   (runR ae ad (k . (*2)))
    runR (ArrayEltRvec3 ae)        (AD_V3 ad)        k = liftM  AD_V3   (runR ae ad (k . (*3)))
    runR (ArrayEltRvec4 ae)        (AD_V4 ad)        k = liftM  AD_V4   (runR ae ad (k . (*4)))
    runR (ArrayEltRvec8 ae)        (AD_V8 ad)        k = liftM  AD_V8   (runR ae ad (k . (*8)))
    runR (ArrayEltRvec16 ae)       (AD_V16 ad)       k = liftM  AD_V16  (runR ae ad (k . (*16)))
    runR (ArrayEltRpair aeR2 aeR1) (AD_Pair ad2 ad1) k = liftM2 AD_Pair (runR aeR2 ad2 k) (runR aeR1 ad1 k)

