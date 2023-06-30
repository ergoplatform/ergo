package org.ergoplatform.wallet.boxes

import java7.compat.Math
import org.ergoplatform.ErgoBoxCandidate
import special.collection.Extensions._

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.util.Try

object ErgoBoxAssetExtractor {
  val MaxAssetsPerBox = 255

  /**
    * Extracts a mapping (assets -> total amount) from a set of boxes passed as a parameter.
    * That is, the method is checking amounts of assets in the boxes(i.e. that a box contains positive
    * amount for an asset) and then summarize and group their corresponding amounts.
    *
    * @param boxes - boxes to check and extract assets from
    * @return a mapping from asset id to balance and total assets number
    */
  def extractAssets(
    boxes: IndexedSeq[ErgoBoxCandidate]
  ): Try[(Map[Seq[Byte], Long], Int)] = Try {
    val map: mutable.Map[Seq[Byte], Long] = mutable.Map[Seq[Byte], Long]()
    val assetsNum = boxes.foldLeft(0) {
      case (acc, box) =>
        require(
          box.additionalTokens.length <= ErgoBoxAssetExtractor.MaxAssetsPerBox,
          "too many assets in one box"
        )
        box.additionalTokens.foreach {
          case (assetId, amount) =>
            val aiWrapped = ArraySeq.unsafeWrapArray(assetId.toArray)
            val total     = map.getOrElse(aiWrapped, 0L)
            map.put(aiWrapped, Math.addExact(total, amount))
        }
        acc + box.additionalTokens.size
    }
    map.toMap -> assetsNum
  }

  /**
    * Cost of assets preservation rules checks.
    * We iterate through all assets to create a map (cost: `(outAssetsNum + inAssetsNum) * tokenAccessCost)`)
    * and after that we iterate through unique asset ids to check preservation rules
    *     (cost: `(inAssets.size + outAssets.size) * tokenAccessCost`)
    * @param inAssetsNum number of input assets in all boxes
    * @param inAssetsSize number if unique input asset ids
    * @param outAssetsNum number of output assets in all boxes
    * @param outAssetsSize number if unique output asset ids
    * @param tokenAccessCost access cost for a token
    * @return total assets access cost
    */
  def totalAssetsAccessCost(
    inAssetsNum: Int,
    inAssetsSize: Int,
    outAssetsNum: Int,
    outAssetsSize: Int,
    tokenAccessCost: Int
  ): Int = {
    val allAssetsCost = Math.multiplyExact(Math.addExact(outAssetsNum, inAssetsNum), tokenAccessCost)
    val uniqueAssetsCost = Math.multiplyExact(Math.addExact(inAssetsSize, outAssetsSize), tokenAccessCost)
    Math.addExact(allAssetsCost, uniqueAssetsCost)
  }

  /**
    * Extract total assets access cost from in/out boxes
    * @param inputBoxes Input boxes
    * @param outputBoxes Output boxes
    * @param tokenAccessCost access cost for a token
    * @return total assets access cost
    */
  def extractTotalAssetsAccessCost(
    inputBoxes: IndexedSeq[ErgoBoxCandidate],
    outputBoxes: IndexedSeq[ErgoBoxCandidate],
    tokenAccessCost: Int
  ): Try[Int] =
    extractAssets(inputBoxes).flatMap {
      case (inAssets, inAssetsNum) =>
        extractAssets(outputBoxes).map {
          case (outAssets, outAssetsNum) =>
            totalAssetsAccessCost(
              inAssetsNum,
              inAssets.size,
              outAssetsNum,
              outAssets.size,
              tokenAccessCost
            )

        }
    }
}
