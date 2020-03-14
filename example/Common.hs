module Common where

import qualified Foreign.Marshal.Array as Array
import Foreign.C.Types (CUInt)
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)


withArrayLen :: (Storable a) => [a] -> (CUInt -> Ptr a -> IO b) -> IO b
withArrayLen xs act =
   Array.withArrayLen xs $ \len ptr -> act (fromIntegral len) ptr

noResult :: IO () -> IO ()
noResult = id
