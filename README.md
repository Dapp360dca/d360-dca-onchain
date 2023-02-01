# DApp360 DCA Plutus contract

This repo is forked from ADAPhilippines/plutus-v2-starter-kit
Only Contract.hs is being worked on now. The project structure will be considered later.

## Minimum Viable Product

The current MVP implementation is described here. The full implementation is [here](https://github.com/Dapp360dca/.github-private)

## General Idea

A user locks some **ammount of ADA** in the PlutusV2 validator script (dcaScript) UTxO and specifies **how much of ADA** should be swapped to **TokenB** with **specified periodicity**. Swapped asset **TokenB** goes directly to the user's wallet. Any time a user can close his posiotion, i.e. withdraw all funds from the dcaScript.

Anyone can perform swaps on a dedicated page. A user performing swap will be rewarded with a small *botFee*

## Validator Script Logic

The dcaScript validator has 2 endpoints (actions) defined by Redeemer:
 - swapDCA to perform swap from ADA (locked in dcaScript UTxO) to TokenB. Can be done by anyone.
 - closeDCA to withdraw all funds from the dcaScript UTxO.

Regardless of the endpoint the validator must not allow malicious withdrawal of funds

Opening DCA Position does not consume any UTxO from the dcaScript, hence it doen't trigger the validator. However the offchain code must ensure that only 1 DCA Position is open for the same owner *(or validator should be updated to protect from doublespending)*

### CloseDCA

The redeemer is parametrized by **TxOutRef** of the UTxO to be closed. So only 1 UTxO can be closed at a time (1 should exist)
The validator must ensure that:
 - The UTxO being consumed is **TxOutRef**
 - All tx outputs go to the owner. As a consequence, only owner can build this tx (not 100% sure about this)
 - Tx is signed by owner

### SwapDCA

This Tx can be submitted by anyone. 
This Tx can consume several UTxOs (for several owners) in the same Tx. The validator is run separately for every UTxO consumed. So validator logic is per 1 UTxO currently being validated.
The validator must ensure that:
 - The correct **amount** is sent to DEX and the **change - fees** goes back to the dcaScript.
  - Output to DEX = swapAmmount + 2 Ada Batcher fee + 2 Ada collateral
  - Change back to dcaScript = ammountLockedAtUTxO - outputToDEX - botFee - txNetworkFee
 - The DEX Datum is constructed correctly and the swap result will go to the owner.
 - The Change Datum back to dcaScript is constructed correctly.
 - The now() is greater then nextSwap. So the swap is allowed.