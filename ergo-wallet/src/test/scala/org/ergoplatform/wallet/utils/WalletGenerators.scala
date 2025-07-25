package org.ergoplatform.wallet.utils

import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId, TokenId}
import org.ergoplatform.sdk.wallet.secrets._
import org.ergoplatform.sdk.wallet.settings.EncryptionSettings
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.TrackedBox
import org.ergoplatform.wallet.mnemonic.{Mnemonic, WordList}
import org.ergoplatform._
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.authds.ADKey
import scorex.util._
import sigma.Extensions.ArrayOps
import sigma.ast.{ByteArrayConstant, ErgoTree, EvaluatedValue, FalseLeaf, SByte, SType, TrueLeaf}
import sigma.ast.syntax.CollectionConstant
import sigma.crypto.CryptoFacade.SecretKeyLength
import sigma.data.ProveDlog
import sigmastate.eval.Extensions._
import sigmastate.helpers.TestingHelpers._
import sigma.eval.Extensions._

object WalletGenerators {

  val MinValuePerByteIncreaseTest: Byte = 2
  val CoinsTotalTest = 9500000000000L

  val passwordGen: Gen[String] = Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.toString)
  val dataGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Gen.posNum[Byte]).map(_.toArray)

  val encryptionSettingsGen: Gen[EncryptionSettings] = for {
    prf <- Gen.oneOf(Seq("HmacSHA1", "HmacSHA256", "HmacSHA512"))
    c <- Gen.posNum[Int]
  } yield EncryptionSettings(prf, c, 256)

  val mnemonicGen: Gen[Mnemonic] = for {
    lang <- Gen.oneOf(WordList.AvailableLanguages)
    strength <- Gen.oneOf(Mnemonic.AllowedStrengths)
  } yield new Mnemonic(lang, strength)

  val entropyGen: Gen[Array[Byte]] = Gen.oneOf(Mnemonic.AllowedEntropyLengths).map(scorex.utils.Random.randomBytes)

  val derivationPathGen: Gen[DerivationPath] = for {
    isPublic <- Gen.oneOf(Seq(true, false))
    indices <- Gen.listOf(Gen.oneOf(Seq(true, false))
      .flatMap(x => Gen.posNum[Int].map(i => if (x) Index.hardIndex(i) else i)))
  } yield DerivationPath(0 +: indices, isPublic)

  val heightGen: Gen[Int] = Gen.choose(0, Int.MaxValue / 2)

  val boxIndexGen: Gen[Short] = for {
    v <- Gen.chooseNum(0, Short.MaxValue)
  } yield v.toShort


  def genLimitedSizedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  def genExactSizeBytes(size: Int): Gen[Array[Byte]] = genLimitedSizedBytes(size, size)

  val boxIdGen: Gen[BoxId] = {
    val x = ADKey @@ genExactSizeBytes(sdk.wallet.Constants.ModifierIdLength)
    x
  }

  val modIdGen: Gen[ModifierId] = genExactSizeBytes(sdk.wallet.Constants.ModifierIdLength).map(bytesToId)

  val assetGen: Gen[(TokenId, Long)] = for {
    id <- boxIdGen
    amt <- Gen.oneOf(1, 500, 20000, 10000000, Long.MaxValue)
  } yield id.toTokenId -> amt

  def additionalTokensGen(cnt: Int): Gen[Seq[(TokenId, Long)]] = Gen.listOfN(cnt, assetGen)

  def additionalTokensGen: Gen[Seq[(TokenId, Long)]] = for {
    cnt <- Gen.chooseNum[Int](0, 10)
    assets <- additionalTokensGen(cnt)
  } yield assets

  val byteArrayConstGen: Gen[CollectionConstant[SByte.type]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield ByteArrayConstant(bytes.toArray)

  def evaluatedValueGen: Gen[EvaluatedValue[SType]] = for {
    arr <- byteArrayConstGen
    v <- Gen.oneOf(TrueLeaf, FalseLeaf, arr)
  } yield v.asInstanceOf[EvaluatedValue[SType]]

  def additionalRegistersGen(cnt: Byte): Gen[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]] = {
    Gen.listOfN(cnt, evaluatedValueGen) map { values =>
      ErgoBox.nonMandatoryRegisters.take(cnt).zip(values).toMap
    }
  }

  def additionalRegistersGen: Gen[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]] = for {
    cnt <- Gen.choose(0: Byte, ErgoBox.nonMandatoryRegistersCount)
    registers <- additionalRegistersGen(cnt)
  } yield registers

  def validValueGen: Gen[Long] = {
    //there are outputs in tests of 183 bytes, and maybe in some tests at least 2 outputs are required
    //thus we put in an input a monetary value which is at least enough for storing 400 bytes of outputs
    val minValue = MinValuePerByteIncreaseTest * 400
    Gen.choose(minValue, CoinsTotalTest / 1000)
  }

  def ergoBoxGen(propGen: Gen[ErgoTree] = Gen.const(ErgoTree.fromProposition(TrueLeaf.toSigmaProp)),
                 tokensGen: Gen[Seq[(TokenId, Long)]] = additionalTokensGen,
                 valueGenOpt: Option[Gen[Long]] = None,
                 heightGen: Gen[Int] = heightGen): Gen[ErgoBox] = for {
    h <- heightGen
    prop <- propGen
    transactionId: Array[Byte] <- genExactSizeBytes(sdk.wallet.Constants.ModifierIdLength)
    boxId: Short <- boxIndexGen
    ar <- additionalRegistersGen
    tokens <- tokensGen
    value <- valueGenOpt.getOrElse(validValueGen)
  } yield {
    val box = testBox(value, prop, h, tokens, ar, transactionId.toModifierId, boxId)
    if (box.bytes.length < ErgoBox.MaxBoxSize) {
      box
    } else {
      // is size limit is reached, generate box without registers and tokens
      testBox(value, prop, h, Seq(), Map(), transactionId.toModifierId, boxId)
    }
  }

  val ergoBoxGen: Gen[ErgoBox] = ergoBoxGen()

  def derivationPathGen(isPublic: Boolean, allowHardened: Boolean): Gen[DerivationPath] = for {
    indices <- Gen.listOf(Gen.oneOf(Seq(true, false))
      .flatMap(x => Gen.posNum[Int].map(i => if (x && allowHardened) Index.hardIndex(i) else i)))
  } yield DerivationPath(0 +: indices, isPublic)

  def extendedSecretGen: Gen[ExtendedSecretKey] = for {
    seed <- Gen.const(SecretKeyLength).map(scorex.utils.Random.randomBytes)
  } yield ExtendedSecretKey.deriveMasterKey(seed, usePre1627KeyDerivation = false)

  def extendedPubKeyGen: Gen[ExtendedPublicKey] = extendedSecretGen.map(_.publicKey)

  def extendedPubKeyListGen: Gen[Seq[ExtendedPublicKey]] = extendedSecretGen.flatMap { sk =>
    Gen.choose(1, 100).map { cnt =>
      (1 to cnt).foldLeft(IndexedSeq(sk)) { case (keys, _) =>
        val dp = DerivationPath.nextPath(keys, usePreEip3Derivation = false).get
        val newSk = sk.derive(dp)
        keys :+ newSk
      }.map(_.publicKey)
    }
  }

  def appStatusesGen: Gen[Set[ScanId]] = {
    if(scala.util.Random.nextBoolean()) {
      // simulate complex usage scenario
      Gen.nonEmptyListOf(Gen.posNum[Short]).map(_.map { id: Short => ScanId @@ id }.toSet)
    } else {
      // simulate simple payment
      Set(PaymentsScanId)
    }
  }

  def trackedBoxGen: Gen[TrackedBox] = for {
    creationTxId <- modIdGen
    creationOutIndex <- boxIndexGen
    inclusionHeightOpt <- Gen.option(heightGen)
    spendingTxIdOpt <- Gen.option(modIdGen)
    spendingHeightOpt <- Gen.option(heightGen)
    box <- ergoBoxGen
    appStatuses <- appStatusesGen
  } yield TrackedBox(
    creationTxId, creationOutIndex, inclusionHeightOpt, spendingTxIdOpt, spendingHeightOpt, box, appStatuses)


  def unsignedTxGen(secret: SecretKey): Gen[(IndexedSeq[ErgoBox], UnsignedErgoLikeTransaction)] = {
    val dlog: Gen[ErgoTree] = Gen.const(ErgoTree.fromProposition(secret.privateInput.publicImage.asInstanceOf[ProveDlog]))

    for {
      ins <- Gen.listOfN(2, ergoBoxGen(dlog))
      value <- Gen.posNum[Long]
      h <- Gen.posNum[Int]
      out = new ErgoBoxCandidate(
        value,
        ErgoTreePredef.feeProposition(),
        h,
        Array.empty[(ErgoBox.TokenId, Long)].toColl,
        Map.empty
      )
      unsignedInputs = ins
        .map { box =>
          new UnsignedInput(box.id)
        }
        .toIndexedSeq
      unsignedTx = new UnsignedErgoLikeTransaction(
        unsignedInputs,
        IndexedSeq(),
        IndexedSeq(out)
      )
    } yield (ins.toIndexedSeq, unsignedTx)
  }

}
