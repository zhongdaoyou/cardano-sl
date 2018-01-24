{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

-- | This module is dedicated to support the verification granularity proposal.
-- See docs/proposals/serialization.md.

module Pos.Util.Verification
    ( Ver(..)
    , Verifiable(..)
    , verField
    ) where

import           Universum

import           Data.Coerce (Coercible, coerce)

-- | Verification datatype. For now we support only two levels of data
-- verification.
data Ver = Ver | Unver

-- | Typeclass for verifiable things. It's easier to use than number
-- of 'verifyX' functions for every datatype 'X' that has verifiable
-- capabilities.
class Verifiable (c :: Ver -> *) where
    -- | Verify value -- return it or error message.
    toVerified :: c 'Unver -> Either Text (c 'Ver)
    -- |
    toUnverified :: c 'Ver -> c 'Unver

    -- | For types that have @c@ as a phantom type, toUnverified is
    -- pretty straightforward.
    default toUnverified :: Coercible (c 'Ver) (c 'Unver) => c 'Ver -> c 'Unver
    toUnverified = coerce

-- | Verifies some field, prefixing with the text value in case of
-- error. Prefix is supposed to be the record field name.
verField :: (Verifiable c) => Text -> c 'Unver -> Either Text (c 'Ver)
verField p c = first (\x -> p <> "." <> x) $ toVerified c

-- ----------------------------------------------------------------------------
-- -- Instances
-- ----------------------------------------------------------------------------
--
-- instance Verifiable c => Verifiable [c] where
--     toVerified = mapM $ verField "listElem"
--     toUnverified = map toUnverified
