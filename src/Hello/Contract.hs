{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass      #-}


module Hello.Contract (validator, wrapped, serialized, hash, DcaDatum (..), DcaRedeemer (..)) where

import qualified    Prelude                         as Haskell
import              Cardano.Api.Shelley             (PlutusScript (..))
import              Codec.Serialise                 (serialise)
import qualified    Data.ByteString.Lazy            as BSL
import qualified    Data.ByteString.Short           as BSS
import              Hello.Shared                    (validatorHash, wrap)
import qualified    Plutus.V1.Ledger.Scripts        as Scripts
import qualified    Plutus.V2.Ledger.Api            as Api
import              PlutusTx
import              PlutusTx.Prelude
import              Cardano.Api
import qualified    Plutus.V1.Ledger.Value          as V
import              Data.Aeson                      (Value(Bool))
import qualified    Plutus.V2.Ledger.Contexts       as Api
import              Codec.Serialise.Encoding        (Tokens(TkInteger))

-- | Inline Datum defining DCA position locked in a validator script UTxO
data DcaDatum = DcaDatum {
                    owner       :: Api.Address      -- ^ Owner of the DCA position UTxO. Only owner can close DCA position.
                ,   fromAsset   :: V.AssetClass     -- ^ Swap from this Asset
                ,   toAsset     :: V.AssetClass     -- ^ Swap to this Asset
                ,   amount      :: Integer          -- ^ Ammount of fromAsset (TODO: or USD) to be swapped each time
                ,   nextSwap    :: Integer          -- ^ Earliest time of next swap = last swap + period (POSIXTIME)
                ,   period      :: Integer          -- ^ Period of swap in milliseconds
                } deriving (Eq)
PlutusTx.unstableMakeIsData ''DcaDatum

-- | Redeemer Action for dcaValidator
data DcaRedeemer
    = CloseDCA Api.TxOutRef -- ^ Consume DCA Position UTxO to user's wallet. Only 1 DCA Position can be closed at a time.
    | SwapDCA                    -- ^ For each consumed UTxO, only 'amount' is sent to DEX script, the rest goes back to DCA position
PlutusTx.unstableMakeIsData ''DcaRedeemer

-- | Validator for consuming DCA position UTxO
{-# INLINABLE dcaValidator #-}
dcaValidator :: DcaDatum -> DcaRedeemer -> Api.ScriptContext -> Bool
dcaValidator datum action ctx = case action of
    CloseDCA oref   -> checkClose oref
    SwapDCA         -> checkSwap

    where
        txInfo :: Api.TxInfo
        txInfo = Api.scriptContextTxInfo ctx

        txInputs :: [Api.TxInInfo]
        txInputs = Api.txInfoInputs txInfo

        txOutputs :: [Api.TxOut]
        txOutputs = Api.txInfoOutputs txInfo

        utxoBeingConsumed :: Api.TxOutRef
        utxoBeingConsumed = let Just i = Api.findOwnInput ctx
                            in Api.txInInfoOutRef i

        ownValue :: V.Value     -- ^ Value locked at consumed dcaScript UTxO
        ownDatum :: DcaDatum   -- ^ Datum at consumed dcaScript UTxO
        (ownValue, ownDatum) =  let Just i = Api.findOwnInput ctx
                                    out = Api.txInInfoResolved i
                                in case Api.txOutDatum out of
                                     Api.OutputDatum d -> case PlutusTx.fromBuiltinData $ Api.getDatum d of
                                        Just dcaDatum -> (Api.txOutValue out, dcaDatum)

        ownOutputDatum :: DcaDatum -- ^ Updated inline datum attached to change that is returned back to dcaScript
        ownOutputValue :: V.Value   -- ^ Change that is returned back to dcaScript
        (ownOutputValue, ownOutputDatum) = case Api.getContinuingOutputs ctx of
            [o] -> let (Api.OutputDatum d) = Api.txOutDatum o in case PlutusTx.fromBuiltinData $ Api.getDatum d of
                    Just dcaDatum -> (Api.txOutValue o, dcaDatum)            

        dcaOwner :: Api.Address
        dcaOwner = owner datum

        outputsToOwnerOnly :: Bool -- ^ For CloseDCA all outputs must go to dcaOwner
        outputsToOwnerOnly =   all (\o -> Api.txOutAddress o == dcaOwner) txOutputs

        checkClose oref
            =   traceIfFalse "Specified UTxO is not consumed" (utxoBeingConsumed == oref)
            &&  traceIfFalse "Not all outputs go to dcaOwner" outputsToOwnerOnly

        checkValueToDEX :: Bool -- ^ Value sent to DEX corresponds to amount
        checkValueToDEX = True

        checkDatumToDEX :: Bool -- ^ Datum is correctly formatted for DEX
        checkDatumToDEX = True

        checkOutputToDEX :: Bool -- ^ Correct output to DEX
        checkOutputToDEX = checkValueToDEX && checkDatumToDEX
        
        swapFees :: Integer -- ^ network fee + batcher fee 2 ADA + collateral 2 ADA + dcaBot fee
        swapFees = 5
        
        checkOwnValue :: Bool -- ^ Correct change is returned back to dcaScript
        checkOwnValue = True

        checkOwnDatum :: Bool -- ^ amount and nextSwap are correctly updated in dcaDatum
        checkOwnDatum = ownOutputDatum == newDatum
            where 
                newNextSwap = nextSwap datum + period datum
                ownValueAmount = V.assetClassValueOf ownValue $ fromAsset datum
                newAmount = if (V.unAssetClass . fromAsset $ datum) == ("","") 
                                then ownValueAmount - amount datum - swapFees
                                else ownValueAmount - amount datum
                newDatum = datum {amount = newAmount, nextSwap = newNextSwap}


        checkContinuingOutput :: Bool -- ^ Continuing output value and datum are correct
        checkContinuingOutput = checkOwnValue && checkOwnDatum

        checkTimeValidity :: Bool -- ^ Now is more than nextSwap
        checkTimeValidity = True

        checkSwap
            =   checkOutputToDEX             
            &&  checkContinuingOutput
            &&  checkTimeValidity


wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped = wrap dcaValidator

validator :: Api.Validator
validator = Api.mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])

serialized :: PlutusScript PlutusScriptV2
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator

hash :: Scripts.ValidatorHash
hash = validatorHash validator
