{-# LANGUAGE OverloadedStrings #-}

module PlutusUtils (showIntegerBuiltinString) where

import           PlutusTx.Builtins (addInteger, divideInteger,
                                    greaterThanEqualsInteger, lessThanInteger,
                                    modInteger, multiplyInteger)
import           PlutusTx.Prelude  as PP
import qualified Prelude           hiding (($))

{-# INLINEABLE showIntegerBuiltinString #-}
showIntegerBuiltinString :: Integer -> BuiltinString
showIntegerBuiltinString i = showIntegerBuiltinString' i
  where
      showIntegerBuiltinString' i
          | i `greaterThanEqualsInteger` 10     = appendString (showIntegerBuiltinString (i `divideInteger` 10))
                                                              (decodeUtf8 $ consByteString ((i `modInteger` 10)  `addInteger` 0x30) emptyByteString)
          | i `lessThanInteger` 0               = appendString "-" $ showIntegerBuiltinString $ PP.negate 1 `multiplyInteger` i
          | otherwise                           = decodeUtf8 $ consByteString ((i `modInteger` 10)  `addInteger` 0x30) emptyByteString

