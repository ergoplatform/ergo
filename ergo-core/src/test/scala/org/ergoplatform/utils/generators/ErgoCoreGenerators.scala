package org.ergoplatform.utils.generators

import com.google.common.primitives.Shorts
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.mining.{AutolykosSolution, genPk, q}
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ModePeerFeature
import org.ergoplatform.nodeView.history.{ErgoSyncInfo, ErgoSyncInfoV1, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Constants, ErgoValidationSettings, ErgoValidationSettingsUpdate, ValidationRules}
import org.ergoplatform.testkit.generators.CoreGenerators
import org.ergoplatform.utils.ErgoTestConstants
import sigma.validation.{ChangedRule, DisabledRule, EnabledRule, ReplacedRule}
import org.ergoplatform.wallet.utils.Generators
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32
import sigma.ast.ErgoTree
import sigma.crypto.{CryptoConstants, EcPointType}
import sigma.data.{ProveDHTuple, ProveDlog}
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.crypto.DiffieHellmanTupleProverInput
import sigma.interpreter.ProverResult

import scala.util.Random

object ErgoCoreGenerators {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  lazy val trueLeafGen: Gen[ErgoTree] = Gen.const(Constants.TrueLeaf)
  lazy val falseLeafGen: Gen[ErgoTree] = Gen.const(Constants.FalseLeaf)

  lazy val smallPositiveInt: Gen[Int] = Gen.choose(1, 5)

  lazy val noProofGen: Gen[ProverResult] =
    Gen.const(emptyProverResult)

  lazy val dlogSecretWithPublicImageGen: Gen[(DLogProverInput, ProveDlog)] = for {
    secret <- genBytes(32).map(seed => BigIntegers.fromUnsignedByteArray(seed))
    dlpi = DLogProverInput(secret)
  } yield (dlpi, dlpi.publicImage)

  lazy val dhtSecretWithPublicImageGen: Gen[(DiffieHellmanTupleProverInput, ProveDHTuple)] = for {
    secret <- genBytes(32).map(seed => BigIntegers.fromUnsignedByteArray(seed))
    g <- genECPoint
    h <- genECPoint
    u = CryptoConstants.dlogGroup.exponentiate(g, secret)
    v = CryptoConstants.dlogGroup.exponentiate(h, secret)
    dhtpi = DiffieHellmanTupleProverInput(secret, ProveDHTuple(g, h, u, v))
  } yield (dhtpi, dhtpi.publicImage)

  lazy val proveDlogGen: Gen[ProveDlog] = for {
    seed <- genBytes(32)
  } yield DLogProverInput(BigIntegers.fromUnsignedByteArray(seed)).publicImage

  lazy val proveDlogTreeGen: Gen[ErgoTree] = proveDlogGen.map(dl => ErgoTree.fromSigmaBoolean(dl))

  lazy val ergoPropositionGen: Gen[ErgoTree] = Gen.oneOf(trueLeafGen, falseLeafGen, proveDlogTreeGen)

  lazy val positiveIntGen: Gen[Int] = Gen.choose(1, Int.MaxValue)

  lazy val ergoSyncInfoV1Gen: Gen[ErgoSyncInfoV1] = for {
    ids <- Gen.nonEmptyListOf(modifierIdGen).map(_.take(ErgoSyncInfo.MaxBlockIds))
  } yield ErgoSyncInfoV1(ids)

  lazy val ergoSyncInfoV2Gen: Gen[ErgoSyncInfoV2] = for {
    hds <- Gen.nonEmptyListOf(invalidHeaderGen).map(_.take(5))
  } yield ErgoSyncInfoV2(hds)

  lazy val digest32Gen: Gen[Digest32] = {
    val x = Digest32 @@ genBytes(32)
    x
  }

  lazy val stateRootGen: Gen[ADDigest] = {
    val x = ADDigest @@ genBytes(Constants.ModifierIdSize + 1)
    x
  }

  lazy val serializedAdProofGen: Gen[SerializedAdProof] = {
    val x = SerializedAdProof @@ genBoundedBytes(32, 32 * 1024)
    x
  }

  def genSecureBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    Gen.choose(minSize, maxSize).flatMap {
      scorex.util.Random.randomBytes
    }

  /**
   * Main KV generator: contains special handling for key.head == 0
   */
  def extensionKvGen(keySize: Int, valuesSize: Int): Gen[(Array[Byte], Array[Byte])] = for {
    key <- genSecureBoundedBytes(keySize, keySize)
    value <- if (key.head == 0) genSecureBoundedBytes(4, 4) else genSecureBoundedBytes(valuesSize, valuesSize)
  } yield (key, value)

  /**
   * Random KV generator: all random key/values
   */
  def extensionKvGenImvValue(keySize: Int, valuesSize: Int): Gen[(Array[Byte], Array[Byte])] = for {
    key <- genSecureBoundedBytes(keySize, keySize)
    value <- genSecureBoundedBytes(valuesSize, valuesSize)
  } yield (key, value)

  /**
   * Special KV generator: does not generate key.head == 1, because improperly packed interlink
   * would be generated, leading to failure.
   */
  def extensionKvGenValidMKV(keySize: Int, valueSize: Int): Gen[(Array[Byte], Array[Byte])] = {
    def keyGenHeadNotOne: Gen[Array[Byte]] = genSecureBoundedBytes(keySize, keySize).flatMap { key =>
      if (key.headOption.contains(1.toByte)) keyGenHeadNotOne else Gen.const(key)
    }

    for {
      key <- keyGenHeadNotOne
      value <- genSecureBoundedBytes(valueSize, valueSize)
    } yield (key, value)
  }

  lazy val extensionGen: Gen[Extension] = for {
    headerId <- modifierIdGen
    mandatoryElements <- Gen.mapOf(extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize))
  } yield {
    val me = mandatoryElements
      .map(kv => Shorts.fromByteArray(kv._1) -> kv._2)
      .map(kv => Shorts.toByteArray(kv._1) -> kv._2)
    Extension(headerId, me.toSeq)
  }

  lazy val genECPoint: Gen[EcPointType] = genBytes(32).map(b => genPk(BigInt(b).mod(q)))

  lazy val powSolutionGen: Gen[AutolykosSolution] = for {
    pk <- genECPoint
    w <- genECPoint
    n <- genBytes(8)
    d <- Arbitrary.arbitrary[BigInt].map(_.mod(q - 1) + 1)
  } yield AutolykosSolution(pk, w, n, d)

  /**
    * Generates required difficulty in interval [1, 2^^255]
    **/
  lazy val requiredDifficultyGen: Gen[BigInt] = Arbitrary.arbitrary[BigInt].map(_.mod(BigInt(2).pow(255)).abs + 1)

  /*
    TODO: this generator is used sometimes to construct a full block. A generator for the latter is not generating
     a proper extension section for a beginning of a voting epoch (network parameter values should be in the  extension
     in this case. Currently we're fixing it here with filtering out heights corresponding to the beginning of voting
     epochs. A more careful solution would be to implement proper generators.
   */
  lazy val invalidHeaderGen: Gen[Header] = for {
    version <- Arbitrary.arbitrary[Byte]
    parentId <- modifierIdGen
    stateRoot <- stateRootGen
    adRoot <- digest32Gen
    transactionsRoot <- digest32Gen
    requiredDifficulty <- requiredDifficultyGen
    height <- Gen.choose(1, Int.MaxValue).retryUntil(_ % chainSettings.voting.votingLength != 0)
    powSolution <- powSolutionGen
    timestamp <- positiveLongGen
    extensionHash <- digest32Gen
  } yield Header(
    version,
    parentId,
    adRoot,
    stateRoot,
    transactionsRoot,
    timestamp,
    DifficultySerializer.encodeCompactBits(requiredDifficulty),
    height,
    extensionHash,
    powSolution,
    Array.fill(3)(0: Byte),
    None
  )

  /**
    * Header generator with default miner pk in pow solution
    */
  lazy val defaultHeaderGen: Gen[Header] = invalidHeaderGen.map { h =>
    h.copy(powSolution = h.powSolution.copy(pk = defaultMinerPkPoint))
  }

  lazy val randomADProofsGen: Gen[ADProofs] = for {
    headerId <- modifierIdGen
    proof <- serializedAdProofGen
  } yield ADProofs(headerId, proof)

  lazy val modeFeatureGen: Gen[ModePeerFeature] = for {
    stateTypeCode <- Gen.choose(StateType.Utxo.stateTypeCode, StateType.Utxo.stateTypeCode)
    popowSuffix <- Gen.choose(1, 10)
    blocksToKeep <- Gen.choose(1, 100000)
  } yield ModePeerFeature(
    StateType.fromCode(stateTypeCode),
    Random.nextBoolean(),
    if (Random.nextBoolean()) Some(popowSuffix) else None,
    blocksToKeep)

  lazy val ergoValidationSettingsUpdateGen: Gen[ErgoValidationSettingsUpdate] = for {
    n <- Gen.choose(1, 200)
    disabledRules = ValidationRules.rulesSpec.filter(_._2.mayBeDisabled).keys.take(n).toSeq
    replacedRuleCode <- Gen.choose(sigma.validation.ValidationRules.FirstRuleId, Short.MaxValue)
    changedRuleValue <- genBoundedBytes(0, 127)
    statuses <- Gen.listOf(Gen.oneOf(DisabledRule, EnabledRule, ReplacedRule(replacedRuleCode), ChangedRule(changedRuleValue)))
    statusUpdates = org.ergoplatform.validation.ValidationRules.ruleSpecs.take(statuses.size).map(_.id).zip(statuses)
  } yield ErgoValidationSettingsUpdate(disabledRules, statusUpdates)

  lazy val ergoValidationSettingsGen: Gen[ErgoValidationSettings] = ergoValidationSettingsUpdateGen
    .map(u => ErgoValidationSettings.initial.updated(u))

  /** Random long from 1 to maximum - 1
    *
    * @param maximum should be positive
    */
  def randomLong(maximum: Long = Long.MaxValue): Long = {
    if (maximum < 3) 1 else Math.abs(Random.nextLong()) % (maximum - 2) + 1
  }

}
