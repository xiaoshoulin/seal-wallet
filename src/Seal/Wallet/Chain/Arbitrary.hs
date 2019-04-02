{-# OPTIONS_GHC -fno-warn-orphans #-}

module Seal.Wallet.Chain.Arbitrary
       ( genUpdatePayload
       , genUpdateVote
       , genUpdateProposal
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Test.QuickCheck (Arbitrary (..), Gen, elements, frequency,
                     listOf, listOf1)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Seal.Chain.Update (BlockVersion (..),
                     BlockVersionData (..), BlockVersionModifier,
                     SoftforkRule (..), SystemTag (..),
                     UpdateData (..), UpdatePayload (..), UpdateProposal,
                     UpdateProposalToSign (..), UpdateVote (..),
                     VoteState (..),
                     mkUpdateProposalWSign, mkUpdateVote)
import           Seal.Core.Attributes (mkAttributes)
import           Seal.Crypto (ProtocolMagic, fakeSigner)

import           Test.Seal.Core.Arbitrary ()
import           Test.Seal.Core.Arbitrary.Slotting ()
import           Test.Seal.Crypto.Arbitrary ()
import           Test.Seal.Crypto.Dummy (dummyProtocolMagic)
import           Seal.Wallet.API.V1.Info ()


instance Arbitrary SoftforkRule where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlockVersionData where
    arbitrary = genericArbitrary
    shrink = genericShrink

-- instance Arbitrary ApplicationName where
--     arbitrary =
--         ApplicationName .
--         toText . map selectAlpha . take applicationNameMaxLength <$>
--         arbitrary
--       where
--         selectAlpha n = alphabet !! (n `mod` length alphabet)
--         alphabet = "-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

instance Arbitrary BlockVersion where
    arbitrary = genericArbitrary
    shrink = genericShrink

-- instance Arbitrary SoftwareVersion where
--     arbitrary = genericArbitrary
--     shrink = genericShrink

instance Arbitrary BlockVersionModifier where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SystemTag where
    arbitrary =
        elements . fmap SystemTag $
        (<>) <$> ["win", "linux", "mac"] <*> ["32", "64"]
    shrink = genericShrink

genUpdateVote :: ProtocolMagic -> Gen UpdateVote
genUpdateVote pm = mkUpdateVote pm <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UpdateVote where
    arbitrary = genUpdateVote dummyProtocolMagic
    shrink = genericShrink

genUpdateProposal :: ProtocolMagic -> Gen UpdateProposal
genUpdateProposal pm = do
    upBlockVersion <- arbitrary
    upBlockVersionMod <- arbitrary
    upSoftwareVersion <- arbitrary
    upData <- HM.fromList <$> listOf1 arbitrary
    let upAttributes = mkAttributes ()
    ss <- fakeSigner <$> arbitrary
    pure $
        mkUpdateProposalWSign
            pm
            upBlockVersion
            upBlockVersionMod
            upSoftwareVersion
            upData
            upAttributes
            ss

instance Arbitrary UpdateProposal where
    arbitrary = genUpdateProposal dummyProtocolMagic
    shrink = genericShrink

instance Arbitrary UpdateProposalToSign where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VoteState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary UpdateData where
    arbitrary = genericArbitrary
    shrink = genericShrink

genUpdatePayload :: ProtocolMagic -> Gen UpdatePayload
genUpdatePayload pm =
    UpdatePayload
        <$> genMaybeUpdateProposal
        <*> listOf (genUpdateVote pm)
  where
    -- Arbitrary1 instance for Maybe uses these frequencies.
    genMaybeUpdateProposal = frequency
        [ (1, return Nothing)
        , (3, Just <$> genUpdateProposal pm)
        ]

instance Arbitrary UpdatePayload where
    arbitrary = genUpdatePayload dummyProtocolMagic
    shrink = genericShrink
