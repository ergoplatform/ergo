package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoLikeInterpreter
import scapi.sigma.DLogProtocol.{DLogProverInput, ProveDlog}
import scapi.sigma.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import sigmastate.interpreter.ProverInterpreter
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.CostTable

class ErgoProvingInterpreter(seed: String, override val maxCost: Long = CostTable.ScriptLimit)
  extends ErgoLikeInterpreter(maxCost) with ProverInterpreter {

  def hash256(input: Array[Byte]) = Blake2b256(input)

  //todo: take Blake2b160 ?
  def hash160(input: Array[Byte]) = hash256(input).take(20)

  def bytesToTrack(secret: DLogProverInput): Array[Byte] = bytesToTrack(secret.publicImage)

  def bytesToTrack(pubkey: ProveDlog): Array[Byte] = {
    ValueSerializer.serialize(pubkey)
  }

  def address(pubkey: ProveDlog): String = {
    val bt = bytesToTrack(pubkey)
    val bth160 = hash160(bt)

    //add network identifier
    val withNetworkByte = (0: Byte) +: bth160

    val checksum = hash256(withNetworkByte).take(4)
    Base58.encode(withNetworkByte ++ checksum)
  }

  override lazy val secrets: Seq[SigmaProtocolPrivateInput[_, _]] =
    dlogSecrets ++ dhSecrets

  lazy val dlogSecrets: Seq[DLogProverInput] = ErgoWallet.secretsFromSeed(seed).map(DLogProverInput.apply)

  lazy val dhSecrets: Seq[DiffieHellmanTupleProverInput] =
    (1 to 4).map(_ => DiffieHellmanTupleProverInput.random())
}
