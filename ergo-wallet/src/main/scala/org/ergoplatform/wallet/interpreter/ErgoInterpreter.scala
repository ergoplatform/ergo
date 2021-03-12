package org.ergoplatform.wallet.interpreter

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.validation.ValidationRules
import org.ergoplatform.wallet.protocol.Constants
import org.ergoplatform.wallet.protocol.context.ErgoLikeParameters
import org.ergoplatform.{ErgoLikeContext, ErgoBox, ErgoBoxCandidate, ErgoLikeInterpreter}
import scorex.crypto.authds.ADDigest
import scorex.util.ScorexLogging
import sigmastate.Values.ErgoTree
import sigmastate.eval.{RuntimeIRContext, IRContext}
import sigmastate.interpreter.Interpreter.{VerificationResult, ScriptEnv}
import sigmastate.interpreter.{CacheKey, PrecompiledScriptProcessor, ScriptProcessorSettings, ProcessorStats}
import sigmastate.{AvlTreeData, AvlTreeFlags}

import scala.util.Try

/**
  * ErgoTree language interpreter, Ergo version. In addition to ErgoLikeInterpreter, it contains
  * rules for expired boxes spending validation.
  *
  * @param params - current values of adjustable blockchain settings
  */
class ErgoInterpreter(params: ErgoLikeParameters)(implicit IR: IRContext)
  extends ErgoLikeInterpreter {

  override type CTX = ErgoLikeContext

  /**
    * Checks that expired box is spent in a proper way
    *
    * @param box           - box being spent
    * @param output        - newly created box when storage fee covered, otherwise any output box
    * @param currentHeight - current height of the blockchain (at the moment of spending)
    * @return whether the box is spent properly according to the storage fee rule
    */
  protected def checkExpiredBox(box: ErgoBox, output: ErgoBoxCandidate, currentHeight: Height): Boolean = {
    val storageFee = params.storageFeeFactor * box.bytes.length

    val storageFeeNotCovered = box.value - storageFee <= 0
    val correctCreationHeight = output.creationHeight == currentHeight
    val correctOutValue = output.value >= box.value - storageFee

    // all the registers except of R0 (monetary value) and R3 (creation height and reference) must be preserved
    val correctRegisters = ErgoBox.allRegisters.tail
      .forall(rId => rId == ErgoBox.ReferenceRegId || box.get(rId) == output.get(rId))

    storageFeeNotCovered || (correctCreationHeight && correctOutValue && correctRegisters)
  }

  /**
    * Checks that given exp evaluates to `true`.
    *
    * @param env     - environment to use during expression evaluation
    * @param exp     - expression to check
    * @param context - expression evaluation context
    * @param proof   - cryptographic proof
    * @param message - message
    */
  override def verify(env: ScriptEnv,
                      exp: ErgoTree,
                      context: CTX,
                      proof: Array[Byte],
                      message: Array[Byte]): Try[VerificationResult] = {

    val varId = Constants.StorageIndexVarId
    val hasEnoughTimeToBeSpent = context.preHeader.height - context.self.creationHeight >= Constants.StoragePeriod
    // No spending proof provided and enough time since box creation to spend it
    // In this case anyone can spend the expired box by providing in context extension variable #127 (stored in input)
    //    an index of a recreated box (or index of any box if the value in the expired box isn't enough to pay for the storage fee)
    if (hasEnoughTimeToBeSpent && proof.length == 0 && context.extension.values.contains(varId)) {
      Try {
        val idx = context.extension.values(varId).value.asInstanceOf[Short]
        val outputCandidate = context.spendingTransaction.outputCandidates(idx)
        checkExpiredBox(context.self, outputCandidate, context.preHeader.height) -> Constants.StorageContractCost
      }.recoverWith { case _ =>
        super.verify(env, exp, context, proof, message)
      }
    } else {
      super.verify(env, exp, context, proof, message)
    }
  }

}

object ErgoInterpreter {

  /** Creates an interpreter with the given parameters, [[RuntimeIRContext]] for script processing and
    * the `scriptProcessor` for handling compilation caching and handling pre-compiled scripts.
    */
  def apply(params: ErgoLikeParameters): ErgoInterpreter =
    new ErgoInterpreter(params)(new RuntimeIRContext) {
      override val precompiledScriptProcessor: PrecompiledScriptProcessor = scriptProcessor
    }

  /** Create [[AvlTreeData]] with the given digest and all operations enabled. */
  def avlTreeFromDigest(digest: ADDigest): AvlTreeData = {
    val flags = AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = true)
    AvlTreeData(digest, flags, Constants.HashLength)
  }

  /** Top most frequently used complex scripts (with approx counters shown as of 03.03.2021). */
  val frequentlyUsedScripts: Seq[String] = Array(
    /*563717*/ "1005040004000e36100204a00b08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304",
    /*438238*/"101004020e36100204a00b08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a7017300730110010204020404040004c0fd4f05808c82f5f6030580b8c9e5ae040580f882ad16040204c0944004c0f407040004000580f882ad16d19683030191a38cc7a7019683020193c2b2a57300007473017302830108cdeeac93a38cc7b2a573030001978302019683040193b1a5730493c2a7c2b2a573050093958fa3730673079973089c73097e9a730a9d99a3730b730c0599c1a7c1b2a5730d00938cc7b2a5730e0001a390c1a7730f",
    /*237718*/"10160e201a6a8c16e4b1cc9d73d03183565cfb8e79dd84198cb66beeed7d3463e0da2b9805000500040008cd03b038b0783c899be6b5b98bcf55df573c87cb2e01c16604c174e5a7e6105e848e04040406040204000400040604080402040004000402040405000500050205040100d808d601b1a4d602c2a7d603c6a70405d6047300d605860272047301d6067302d607d9010763d806d609c27207d60a9372097202d60bd805d60bc17207d60cc1a7d60de47203d60e99720c720dd60f92720b720e720fd60ced720a720bd60dd802d60dc672070405d60e93720d7203720ed60eed720c720d720ed608d9010863d806d60adb63087208d60bb2720a7303017205d60c8c720b01d60d93720c7204d60ed801d60e8c720b02720ed60f95720d720e7206720feb02730495ed937201730593b1a57306d802d6097207d60a7208d1edda720901b2a57307008fda720a01b2a5730800da720a01b2a473090095ed937201730a93b1a5730bd803d609da720801b2a4730c00d60ada720801b2a4730d00d60b999a720a72099ada720801b2a5730e00da720801b2a5730f00d1edda720701b2a5731000eded917209731191720a7312ec93720b731393720b7314d17315",
    /*237322*/"10060e2002d1541415c323527f19ef5b103eb33c220ea8b66fcb711806b0037d115d63f204000402040004040e201a6a8c16e4b1cc9d73d03183565cfb8e79dd84198cb66beeed7d3463e0da2b98d803d601e4c6a70507d602d901026393cbc27202e4c6a7070ed6037300ea02eb02cd7201cedb6a01dde4c6a70407e4c6a706077201d1ececedda720201b2a573010093cbc2b2a47302007203edda720201b2a473030093cbc2b2a47304007203afa5d9010463afdb63087204d901064d0e948c7206017305",
    /*118943*/"100d0e201a6a8c16e4b1cc9d73d03183565cfb8e79dd84198cb66beeed7d3463e0da2b9804000e20c9ffb7bf74cd7a0fc2b76baf54b4c6192b0a1689e6b0ea6b5d988447c353a3ee0400040004020504040205000402040204000100d806d601e4c6a70407d6027300d603b2a5730100d604c672030407d6057302d606db6a01ddeb02ea02cd7201d1afa5d9010763afdb63087207d901094d0e948c720901720295e67204d808d607db63087203d608b27207730300d609db6308a7d60ab27209730400d60bd9010b63eded93cbc2720b720593e4c6720b070ecbc2a793c1720bc1a7d60cb2a5730500d60de4c672030507d60ee47204ea02d1edededededededeced938c7208018c720a01919c8c72080273068c720a0293cbc2b2a47307007205edededda720b017203da720b01720c937207db6308720cd801d60f86027202730893b27209730901720fb27207730a01720f93e4c672030607720193e4c6720c0607720193e4c6720c0407720d93e4c6720c0507e4720493c5a7c5b2a4730b0094e47204720deb02ce72067201720e720dce72067201720d720ed1730c",
    /*68902*/"100c040004000e201a6a8c16e4b1cc9d73d03183565cfb8e79dd84198cb66beeed7d3463e0da2b98040005020400040005020402040204000400d805d601e4c6a70507d602d9010263ed93cbc27202e4c6a7070e93c17202c1a7d603b2a5730000d604b2a4730100d6057302ea02eb02cd7201cedb6a01dde4c6a70407e4c6a706077201d1ececededda720201720393c57204c5a7eddad9010663d801d608b2db63087206730300ed938c7208017205928c7208027304017203938cb2db6308720373050002998cb2db63087204730600027307ededda720201720493c5b2a4730800c5a7938cb2db6308b2a4730900730a00017205afa5d901066393b1db63087206730b",
    /*67713*/"100c0e201008c10aea11a275eaac3bffdd08427ec6e80b7512476a89396cd25d415a2de10e206c143ec48214ce2d204c959839c8ddfd0ca030007dba4a95894a0815fe4d41380404040604000400040604080400040205c08db70108cd03b038b0783c899be6b5b98bcf55df573c87cb2e01c16604c174e5a7e6105e848ed803d601b1a4d6027300d6037301eb02d1edecededed937201730293b1a57303dad901046393cbc27204720201b2a573040093cbc2b2a47305007203ededed937201730693b1a57307dad901046393cbc27204720201b2a473080093cbc2b2a47309007203aea5d9010463ed93c27204c2a792c1720499c1a7730a730b",
    /*34511*/"101904000e201a6a8c16e4b1cc9d73d03183565cfb8e79dd84198cb66beeed7d3463e0da2b980e206c143ec48214ce2d204c959839c8ddfd0ca030007dba4a95894a0815fe4d41380400040204000400050004020400040005000400040005000402040004000400050005020502050205040100d806d601e4c6a70407d602b2a5730000d603c672020407d6047301d6057302d606db6a01ddeb02ea02cd7201d1afa5d901076393b1db63087207730395ede6720391b1a47304d80ed607d9010763938cb2db63087207730500017204d60895da72070172028cb2db63087202730600027307d609b2a5730800d60adb63087209d60b95ed91b1720a7309da72070172098cb2720a730a0002730bd60c9a7208720bd60db2a4730c00d60e95da720701720d8cb2db6308720d730d0002730ed60faea4d9010f63eded93cbc2720f720593e4c6720f070ecbc2a793c1720fc1a7d610db6308b2a4730f00d61195ed91b172107310938cb272107311000172048cb27210731200027313d612d9011263eded93cbc27212720593e4c67212070ecbc2a793c17212c1a7d613e4c672020507d614e47203ea02d1edededededededeceded91720c720e937208720bef720fed720fedededed92720e73149272117315937208720b929a720e72119a720c7316909a720e72119a720c7317edda7212017202da721201720993e4c672020607720193e4c672090607720193e4c672090407721393e4c672090507e4720393c5a7c5720d94e472037213eb02ce7206720172147213ce7206720172137214d17318",
    /*15540*/"100c0e20dcd3cdd11102e7cb675fd5185a1685ad007619badcf398864274b137d9f45a9e0e20f8a56076694fe7252fc49c8c7c2d99b878d6146a2f8a1b607793bfa1407a32380402040005c08db701040404000404040205c08db70104060400d805d601d901010493cbc2b2a47201007300d602c5a7d603c2a7d604e4c6a70407d6057301eb02d1ecedededda7201017302dad901060493c5b2a47206007202017303dad9010604d801d608b2a5720600eded93c27208720393c1720899c1a7730493e4c6720804077204017305dad901066393cbc27206720501b2a5730600edededda7201017307dad901060493c5b2a47206007202017308dad9010604d801d608b2a5720600eded93c27208720393c1720899c1a7730993e4c672080407720401730adad901066393cbc27206720501b2a4730b00cd7204",
    /*14982*/"100c0e20dcd3cdd11102e7cb675fd5185a1685ad007619badcf398864274b137d9f45a9e0e200aa0f3bcb01af09ba6ad3cbe442ca253ebd601fbe915450c7a2165ed179a18b00402040005c08db701040404000404040205c08db70104060400d805d601d901010493cbc2b2a47201007300d602c5a7d603c2a7d604e4c6a70407d6057301eb02d1ecedededda7201017302dad901060493c5b2a47206007202017303dad9010604d801d608b2a5720600eded93c27208720393c1720899c1a7730493e4c6720804077204017305dad901066393cbc27206720501b2a5730600edededda7201017307dad901060493c5b2a47206007202017308dad9010604d801d608b2a5720600eded93c27208720393c1720899c1a7730993e4c672080407720401730adad901066393cbc27206720501b2a4730b00cd7204",
    /*14435*/"100c0e20dcd3cdd11102e7cb675fd5185a1685ad007619badcf398864274b137d9f45a9e0e2019323d02836f596329b28b2d380d3cf27a10f55db7b1d33bc4537ab538594bae0402040005c08db701040404000404040205c08db70104060400d805d601d901010493cbc2b2a47201007300d602c5a7d603c2a7d604e4c6a70407d6057301eb02d1ecedededda7201017302dad901060493c5b2a47206007202017303dad9010604d801d608b2a5720600eded93c27208720393c1720899c1a7730493e4c6720804077204017305dad901066393cbc27206720501b2a5730600edededda7201017307dad901060493c5b2a47206007202017308dad9010604d801d608b2a5720600eded93c27208720393c1720899c1a7730993e4c672080407720401730adad901066393cbc27206720501b2a4730b00cd7204"
  )

  /** Script processor which uses [[RuntimeIRContext]] to process graphs.
    * Preforms pre-compilation of the given scripts during instantiation.
    * Keeps pre-compiled data structures for the lifetime of JVM.
    */
  val scriptProcessor: PrecompiledScriptProcessor = {
    /** Script compilation requires an instance of [[SigmaValidationSettings]].
      * The only way to pass it to the CacheLoader is via cache key.
      * So here we augment each script bytes with the instance of validation settings.
      */
    val scriptKeys = frequentlyUsedScripts.map { s =>
      val bytes = ErgoAlgos.decodeUnsafe(s)
      CacheKey(bytes, ValidationRules.currentSettings)
    }
    new PrecompiledScriptProcessor(
      ScriptProcessorSettings(
        predefScripts = scriptKeys,
        maxCacheSize = 500,  // takes up around 150Mb
        recordCacheStats = true,
        reportingInterval = 500
      )) with ScorexLogging {
      override protected def createIR(): IRContext = new RuntimeIRContext

      override protected def onReportStats(stats: ProcessorStats): Unit = {
        log.info(s"Stats: $stats")
      }
    }
  }
}
