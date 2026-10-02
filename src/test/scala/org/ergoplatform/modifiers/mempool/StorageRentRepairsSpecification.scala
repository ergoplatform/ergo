package org.ergoplatform.modifiers.mempool

import org.ergoplatform.modifiers.history.CPreHeader
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, UpcomingStateContext, VotingData}
import org.ergoplatform.settings.Constants.{TrueTree}
import org.ergoplatform.settings.{ChainSettings, Constants, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, ErgoBoxCandidate, Input}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigma.ast.{ErgoTree, ShortConstant}
import sigma.data.Digest32Coll
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.ErgoTreeSerializer

import scala.util.{Failure, Success, Try}

/** Production-wiring and release-boundary fixtures for the v5 storage-rent repairs. */
class StorageRentRepairsSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import sigmastate.helpers.TestingHelpers._

  private val RepairHeight = 1831843
  private val ReemissionDebt = 12000000000L

  private val repairedParameters = Parameters(
    0,
    parameters.parametersTable.updated(Parameters.BlockVersion, Header.Interpreter70Version.toInt),
    ErgoValidationSettingsUpdate.empty)

  private val configuredReemission = settings.chainSettings.reemission.copy(
    checkReemissionRules = true,
    emissionNftId = ModifierId @@ "20fa2bf23962cdf51b07722d6237c0c7b8a44f78856c0f7ec308dc1ef1a92a51",
    reemissionTokenId = ModifierId @@ "d9a2cc8a09abfaed87afacfbb7daee79a6b26f10c6613fc13d3f3953e5521d1a",
    reemissionNftId = ModifierId @@ "d3feeffa87f2df63a7a15b4905e618ae3ce4c69a7975f171bd314d0b877927b8")

  private val enforcingChain = settings.chainSettings.copy(
    addressPrefix = ErgoAddressEncoder.MainnetNetworkPrefix,
    reemission = configuredReemission)
  private val nonEnforcingChain = enforcingChain.copy(
    reemission = configuredReemission.copy(checkReemissionRules = false))
  private val reemissionTokenId = configuredReemission.reemissionTokenIdBytes
  private val payToReemission = configuredReemission.reemissionRules.payToReemission

  private val ownerTree = tree(
    "100204a00b08cd02a1f56716cb8df4feb9371437904b9125b82db939238cd7d948786db33de3139fea02d192a39a8cc7a70173007301")
  private val collectorTree = tree(
    "0008cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")

  private def tree(hex: String): ErgoTree =
    ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(hex).get)

  private def contextWith(params: Parameters,
                          chain: ChainSettings,
                          height: Int = RepairHeight): ErgoStateContext = {
    val predictedHeader = CPreHeader(
      version = params.blockVersion,
      parentId = Header.GenesisParentId,
      timestamp = 0L,
      nBits = Constants.InitialNBits,
      height = height,
      votes = Array.fill(3)(0.toByte),
      minerPk = org.ergoplatform.mining.group.generator)
    UpcomingStateContext(
      Seq.empty,
      None,
      predictedHeader,
      genesisStateDigest,
      params,
      validationSettingsNoIl,
      VotingData.empty)(chain)
  }

  private def storageInput(box: ErgoBox): Input = Input(
    box.id,
    ProverResult(
      Array.emptyByteArray,
      ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))

  private def plainInput(box: ErgoBox): Input =
    Input(box.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))

  private def accepted(tx: ErgoTransaction,
                       context: ErgoStateContext,
                       boxes: Seq[ErgoBox]): Boolean = {
    def boxById(id: ErgoBox.BoxId): Try[ErgoBox] =
      boxes.find(box => java.util.Arrays.equals(id, box.id))
        .map(Success(_))
        .getOrElse(Failure(new NoSuchElementException("unknown box")))
    ErgoState.execTransactions(Seq(tx), context, settings.nodeSettings)(boxById).isValid
  }

  private val v4Enforcing = contextWith(parameters, enforcingChain)
  private val v4NonEnforcing = contextWith(parameters, nonEnforcingChain)
  private val v5Enforcing = contextWith(repairedParameters, enforcingChain)
  private val v5NonEnforcing = contextWith(repairedParameters, nonEnforcingChain)

  property("storage-rent repairs: node and wallet activation versions stay aligned") {
    Header.Interpreter70Version shouldBe
      org.ergoplatform.wallet.protocol.Constants.StorageRentRepairsBlockVersion
  }

  property("storage-rent repairs: live EIP-27 claim flips only at v5") {
    enforcingChain.isMainnet shouldBe true
    val from = testBox(
      63000000000L,
      ownerTree,
      777693,
      Seq((Digest32Coll @@ reemissionTokenId) -> ReemissionDebt),
      Map.empty,
      ModifierId @@ "a1eed80ffd1036add2e5ca5b25b627bd42702e776fab1aed0c3cc2ca1bf756af",
      1)
    val fee = parameters.storageFeeFactor.toLong * from.bytes.length
    val input = storageInput(from)
    val collector = new ErgoBoxCandidate(fee, collectorTree, RepairHeight)
    val burnCompliant = ErgoTransaction(
      IndexedSeq(input),
      IndexedSeq.empty,
      IndexedSeq(
        new ErgoBoxCandidate(from.value - fee - ReemissionDebt, ownerTree, RepairHeight),
        new ErgoBoxCandidate(ReemissionDebt, payToReemission, RepairHeight),
        collector))
    val tokenPreserving = ErgoTransaction(
      IndexedSeq(input),
      IndexedSeq.empty,
      IndexedSeq(
        new ErgoBoxCandidate(from.value - fee, ownerTree, RepairHeight, from.additionalTokens),
        collector))

    Base16.encode(from.id) shouldBe
      "fbf119cbeb73cadc5866f63931599c85e26ea603b9db6d4a97914b88f1f82cfb"
    fee shouldBe 168750000L
    burnCompliant.id shouldBe ModifierId @@
      "d895753f0f1e576fce59dfd2e5f1dd497726310d7bbb6639a26cf58e36799bfd"
    tokenPreserving.id shouldBe ModifierId @@
      "13eed065fc5a95d959fe3618b788dfeb4b5ab909c0bd4a3567c5f2fd0afe2f8d"

    Seq(v4Enforcing, v4NonEnforcing).foreach { context =>
      accepted(burnCompliant, context, Seq(from)) shouldBe false
    }
    Seq(v5Enforcing, v5NonEnforcing).foreach { context =>
      accepted(burnCompliant, context, Seq(from)) shouldBe true
    }

    accepted(tokenPreserving, v4NonEnforcing, Seq(from)) shouldBe true
    accepted(tokenPreserving, v4Enforcing, Seq(from)) shouldBe false
    accepted(tokenPreserving, v5NonEnforcing, Seq(from)) shouldBe false
    accepted(tokenPreserving, v5Enforcing, Seq(from)) shouldBe false
  }

  property("storage-rent repairs: v5 debt payment is independent of local EIP-27 policy") {
    val from = testBox(
      63000000000L,
      ownerTree,
      777693,
      Seq((Digest32Coll @@ reemissionTokenId) -> ReemissionDebt),
      Map.empty,
      ModifierId @@ ("10" * 32),
      0)
    val fee = parameters.storageFeeFactor.toLong * from.bytes.length
    val input = storageInput(from)
    val recreated = new ErgoBoxCandidate(
      from.value - fee - ReemissionDebt,
      ownerTree,
      RepairHeight)

    def claim(paymentTree: ErgoTree): ErgoTransaction = ErgoTransaction(
      IndexedSeq(input),
      IndexedSeq.empty,
      IndexedSeq(
        recreated,
        new ErgoBoxCandidate(ReemissionDebt, paymentTree, RepairHeight),
        new ErgoBoxCandidate(fee, collectorTree, RepairHeight)))

    Seq(v5Enforcing, v5NonEnforcing).foreach { context =>
      accepted(claim(payToReemission), context, Seq(from)) shouldBe true
      accepted(claim(TrueTree), context, Seq(from)) shouldBe false
    }

    val v5WithoutEip27 = contextWith(repairedParameters, settings.chainSettings)
    accepted(claim(payToReemission), v5WithoutEip27, Seq(from)) shouldBe false

    v5Enforcing.storageRentReemissionTokenId shouldBe Some(reemissionTokenId)
    v5NonEnforcing.storageRentReemissionTokenId shouldBe Some(reemissionTokenId)
    v4Enforcing.storageRentReemissionTokenId shouldBe None
    v4NonEnforcing.storageRentReemissionTokenId shouldBe None
    v5WithoutEip27.storageRentReemissionTokenId shouldBe None
    contextWith(
      repairedParameters,
      nonEnforcingChain,
      configuredReemission.activationHeight - 1).shouldCheckReemissionRules shouldBe false
    val atActivation = contextWith(
      repairedParameters,
      enforcingChain,
      configuredReemission.activationHeight)
    atActivation.storageRentReemissionTokenId shouldBe None
    atActivation.shouldCheckReemissionRules shouldBe true
    contextWith(
      repairedParameters,
      nonEnforcingChain,
      configuredReemission.activationHeight).shouldCheckReemissionRules shouldBe true
    contextWith(
      repairedParameters,
      enforcingChain,
      configuredReemission.activationHeight + 1).storageRentReemissionTokenId shouldBe Some(reemissionTokenId)
  }

  property("storage-rent repairs: activation-height injection checks are mandatory under v5") {
    val activationHeight = configuredReemission.activationHeight
    val emissionNftId = configuredReemission.emissionNftIdBytes
    val emissionBox = testBox(
      100001000000000L,
      TrueTree,
      activationHeight,
      Seq.empty,
      Map.empty,
      ModifierId @@ ("14" * 32),
      0)
    val injectionBox = testBox(
      2000000000L,
      TrueTree,
      activationHeight,
      Seq(
        (Digest32Coll @@ emissionNftId) -> 1L,
        (Digest32Coll @@ reemissionTokenId) -> 1L),
      Map.empty,
      ModifierId @@ ("15" * 32),
      0)
    val rewardValue = 1000000000L
    val tx = ErgoTransaction(
      IndexedSeq(plainInput(emissionBox), plainInput(injectionBox)),
      IndexedSeq.empty,
      IndexedSeq(
        new ErgoBoxCandidate(
          emissionBox.value + injectionBox.value - rewardValue,
          TrueTree,
          activationHeight,
          injectionBox.additionalTokens),
        new ErgoBoxCandidate(rewardValue, TrueTree, activationHeight)))
    val inputs = Seq(emissionBox, injectionBox)

    accepted(tx, contextWith(parameters, nonEnforcingChain, activationHeight), inputs) shouldBe true
    accepted(tx, contextWith(parameters, enforcingChain, activationHeight), inputs) shouldBe false
    accepted(tx, contextWith(repairedParameters, nonEnforcingChain, activationHeight), inputs) shouldBe false
    accepted(tx, contextWith(repairedParameters, enforcingChain, activationHeight), inputs) shouldBe false
  }

  property("storage-rent repairs: duplicate re-emission entries use the exact aggregate debt") {
    val from = testBox(
      63000000000L,
      ownerTree,
      777693,
      Seq(
        (Digest32Coll @@ reemissionTokenId) -> 7000000000L,
        (Digest32Coll @@ reemissionTokenId) -> 5000000000L),
      Map.empty,
      ModifierId @@ ("11" * 32),
      0)
    val fee = parameters.storageFeeFactor.toLong * from.bytes.length
    val input = storageInput(from)

    def claim(payment: Long): ErgoTransaction = ErgoTransaction(
      IndexedSeq(input),
      IndexedSeq.empty,
      IndexedSeq(
        new ErgoBoxCandidate(from.value - fee - ReemissionDebt, ownerTree, RepairHeight),
        new ErgoBoxCandidate(payment, payToReemission, RepairHeight),
        new ErgoBoxCandidate(fee + ReemissionDebt - payment, collectorTree, RepairHeight)))

    Seq(v5Enforcing, v5NonEnforcing).foreach { context =>
      accepted(claim(ReemissionDebt), context, Seq(from)) shouldBe true
      accepted(claim(ReemissionDebt - 1), context, Seq(from)) shouldBe false
    }
  }

  property("storage-rent repairs: ordinary claim is unchanged across the version boundary") {
    val from = testBox(
      63000000000L,
      ownerTree,
      777693,
      Seq.empty,
      Map.empty,
      ModifierId @@ ("12" * 32),
      0)
    val fee = parameters.storageFeeFactor.toLong * from.bytes.length
    val claim = ErgoTransaction(
      IndexedSeq(storageInput(from)),
      IndexedSeq.empty,
      IndexedSeq(
        new ErgoBoxCandidate(from.value - fee, ownerTree, RepairHeight),
        new ErgoBoxCandidate(fee, collectorTree, RepairHeight)))

    Seq(v4Enforcing, v4NonEnforcing, v5Enforcing, v5NonEnforcing).foreach { context =>
      accepted(claim, context, Seq(from)) shouldBe true
    }
  }
}
