{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ch04.Chapter04 where

import           Data.Typeable

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

-- typeName @Bool == "Bool"
