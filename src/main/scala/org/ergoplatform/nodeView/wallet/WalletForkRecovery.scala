package org.ergoplatform.nodeView.wallet

import akka.actor.Status
import akka.pattern.StatusReply
import org.ergoplatform.ErgoApp
import org.ergoplatform.core.VersionTag
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool, ChangedState}
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import scorex.util.ModifierId

import scala.util.{Failure, Success, Try}

/** Durable wallet rollback and fork quarantine, owned by the wallet actor. */
private[wallet] trait WalletForkRecovery { this: ErgoWalletActor =>
  protected[wallet] def verifyMissingRollback(state: ErgoWalletState, version: VersionTag): Unit = {
    val branchPoint = org.ergoplatform.core.versionToId(version)
    registryCheckpoint(state) match {
      case Failure(t) =>
        enterDeepForkQuarantine(state, s"registry checkpoint is inconsistent: ${t.getMessage}")
      case Success((walletTip, walletHeight)) if branchPoint == PreGenesisHeader.id =>
        verifyUnretainedCheckpoint(state, walletTip, walletHeight, version)
      case Success((walletTip, walletHeight)) =>
        historyReader.heightOf(branchPoint) match {
          case Some(branchHeight) if walletHeight < branchHeight =>
            def verifyBranchPoint(previousTip: Option[ModifierId]): Unit =
              beginFullChainProbe(state, branchPoint, branchHeight)(
                selectedTip => {
                  if (previousTip.exists(_ != selectedTip)) {
                    verifyMissingRollback(state, version)
                  } else {
                    log.info(s"Wallet is behind rollback version $version on the selected full chain")
                    context.become(loadedWallet(state))
                    pendingChainMessages = 0
                    unstashAll()
                  }
                },
                _ => awaitSupersedingRollback(state,
                  s"rollback version $version is off the selected full chain")
              )
            if (walletTip == PreGenesisHeader.id) verifyBranchPoint(None)
            else beginFullChainProbe(state, walletTip, walletHeight)(
              selectedTip => verifyBranchPoint(Some(selectedTip)),
              _ => awaitSupersedingRollback(state,
                s"wallet tip $walletTip is off the selected full chain")
            )
          case Some(_) =>
            verifyUnretainedCheckpoint(state, walletTip, walletHeight, version)
          case None =>
            context.become(awaitRollbackHeader(state, version, retained = false))
            self ! ContinueFullChainProbe
        }
    }
  }

  private def verifyUnretainedCheckpoint(state: ErgoWalletState,
                                         walletTip: ModifierId,
                                         walletHeight: Int,
                                         version: VersionTag): Unit = {
    if (walletTip == PreGenesisHeader.id) {
      // An empty wallet has no applied checkpoint to roll back.
      context.become(loadedWallet(state))
      pendingChainMessages = 0
      unstashAll()
    } else {
      beginFullChainProbe(state, walletTip, walletHeight)(
        selectedTip => historyReader.ifHolderAppliedFullTip(selectedTip) {
          log.info(s"Ignoring superseded unretained rollback version $version")
          context.become(loadedWallet(state))
          pendingChainMessages = 0
          unstashAll()
        } match {
          case Some(_) => ()
          case None => scheduleFullChainProbeRetry()
        },
        selectedTip => historyReader.ifHolderAppliedFullTip(selectedTip) {
          persistDeepForkQuarantine(state)
        } match {
          case Some(persisted) => finishDeepForkQuarantine(state,
            s"rollback version $version is not retained", persisted)
          case None => scheduleFullChainProbeRetry()
        }
      )
    }
  }

  protected[wallet] def verifyRetainedRollback(state: ErgoWalletState, version: VersionTag): Unit = {
    val target = org.ergoplatform.core.versionToId(version)
    if (target == PreGenesisHeader.id) {
      applyRetainedRollback(state, version, target, WalletDigest.empty.height)
    } else {
      historyReader.heightOf(target) match {
        case Some(targetHeight) =>
          beginFullChainProbe(state, target, targetHeight)(
            _ => applyRetainedRollback(state, version, target, targetHeight),
            _ => awaitSupersedingRollback(state,
              s"retained rollback version $version is off the selected full chain")
          )
        case None =>
          context.become(awaitRollbackHeader(state, version, retained = true))
          self ! ContinueFullChainProbe
      }
    }
  }

  /** The intent must be synced before either LevelDB database is mutated. */
  private def applyRetainedRollback(state: ErgoWalletState,
                                    version: VersionTag,
                                    target: ModifierId,
                                    targetHeight: Int): Unit = {
    registryCheckpoint(state) match {
      case Failure(t) =>
        enterDeepForkQuarantine(state, s"registry checkpoint is inconsistent: ${t.getMessage}")
      case Success((source, sourceHeight)) =>
        retainedRollbackSourceHeight = Some(sourceHeight)
        state.storage.beginRetainedRollback(source, target) match {
          case Failure(t) =>
            failClosedRollback(state, s"Could not persist rollback intent for $version", t)
          case Success(_) =>
            rollbackRetainedRegistry(state, version) match {
              case Failure(t) =>
                failClosedRollback(state, s"Could not durably roll back registry to $version", t)
              case Success(_) =>
                if (target == PreGenesisHeader.id) {
                  completeRetainedRollback(state, source, target, targetHeight, None)
                } else {
                  // The selected full tip can change while the two stores are written.
                  beginFullChainProbe(state, target, targetHeight)(
                    selectedTip => completeRetainedRollback(state, source, target,
                      targetHeight, Some(selectedTip)),
                    _ => failClosedRollback(state,
                      s"Rolled-back version $version left the selected full chain",
                      new IllegalStateException("Selected full-chain tip changed during rollback"))
                  )
                }
            }
        }
    }
  }

  private def completeRetainedRollback(state: ErgoWalletState,
                                       source: ModifierId,
                                       target: ModifierId,
                                       targetHeight: Int,
                                       selectedTip: Option[ModifierId]): Unit = {
    registryCheckpoint(state) match {
      case Success((committedTip, committedHeight))
        if committedTip == target && committedHeight == targetHeight =>
        val clearResult = selectedTip match {
          case Some(tip) => historyReader.ifHolderAppliedFullTip(tip) {
            clearRetainedRollbackIntent(state, source, target)
          }
          case None => Some(clearRetainedRollbackIntent(state, source, target))
        }
        clearResult match {
          case None =>
            beginFullChainProbe(state, target, targetHeight)(
              tip => completeRetainedRollback(state, source, target, targetHeight, Some(tip)),
              _ => failClosedRollback(state, s"Rolled-back version $target is no longer selected",
                new IllegalStateException("Selected full-chain tip changed during rollback"))
            )
          case Some(Failure(t)) =>
            failClosedRollback(state, s"Could not clear rollback intent for $target", t)
          case Some(Success(_)) =>
            retainedRollbackSourceHeight = None
            val rolledBackState = state.copy(outputsFilter = None)
            if (!startSupersedingRollback(rolledBackState)) {
              context.become(loadedWallet(rolledBackState))
              pendingChainMessages = 0
              unstashAll()
            }
        }
      case Success((committedTip, committedHeight)) =>
        failClosedRollback(state,
          s"Rollback committed $committedTip at $committedHeight instead of $target at $targetHeight",
          new IllegalStateException("Wallet rollback checkpoint differs from selected target"))
      case Failure(t) =>
        failClosedRollback(state, s"Could not verify rollback checkpoint for $target", t)
    }
  }

  private def failClosedRollback(state: ErgoWalletState, detail: String, cause: Throwable): Unit = {
    val reason = new IllegalStateException(detail, cause)
    log.error(detail, cause)
    context.become(quarantinedWallet(state, reason))
    pendingChainMessages = 0
    unstashAll()
    ErgoApp.shutdownSystem()(context.system)
  }

  protected[wallet] def enterDeepForkQuarantine(state: ErgoWalletState, detail: String): Unit = {
    finishDeepForkQuarantine(state, detail, persistDeepForkQuarantine(state))
  }

  protected[wallet] def finishDeepForkQuarantine(state: ErgoWalletState,
                                       detail: String,
                                       persisted: Try[Unit]): Unit = {
    val reason = persisted match {
      case Success(_) => new IllegalStateException(s"Wallet deep-fork quarantine: $detail")
      case Failure(t) =>
        log.error("Could not persist wallet deep-fork quarantine marker", t)
        new IllegalStateException("Wallet deep-fork quarantine marker could not be persisted", t)
    }
    log.error(reason.getMessage)
    context.become(quarantinedWallet(state, reason))
    pendingChainMessages = 0
    unstashAll()
    if (persisted.isFailure) ErgoApp.shutdownSystem()(context.system)
  }

  /** Keep the prior registry for inspection, but never expose it as the selected chain. */
  protected[wallet] def quarantinedWallet(state: ErgoWalletState, reason: Throwable): Receive = {
    case GetWalletStatus =>
      sender() ! WalletStatus(
        Try(state.secretIsSet(settings.walletSettings.testMnemonic)).getOrElse(false) ||
          Try(JsonSecretStorage.readFile(settings.walletSettings.secretStorage).isSuccess).getOrElse(false),
        false,
        None,
        retainedRollbackSourceHeight.getOrElse(Try(state.getWalletHeight).getOrElse(0)),
        Some(reason.getMessage)
      )
    case CloseWallet =>
      state.storage.close()
      state.registry.close()
      context stop self
    case InitWallet(walletPass, mnemonicPassOpt) =>
      walletPass.erase()
      mnemonicPassOpt.foreach(_.erase())
      sender() ! Failure(reason)
    case RestoreWallet(mnemonic, mnemonicPassOpt, walletPass, _) =>
      mnemonic.erase()
      mnemonicPassOpt.foreach(_.erase())
      walletPass.erase()
      sender() ! Failure(reason)
    case UnlockWallet(walletPass) =>
      walletPass.erase()
      sender() ! Failure(reason)
    case CheckSeed(mnemonic, passOpt) =>
      mnemonic.erase()
      passOpt.foreach(_.erase())
      sender() ! Status.Failure(reason)
    case RescanWallet(_) => sender() ! Failure(reason)
    case GetPrivateKeyFromPath(_) => sender() ! Failure(reason)
    case GetFirstSecret => sender() ! FirstSecretResponse(Failure(reason))
    case UpdateChangeAddress(_) => sender() ! StatusReply.error(reason)
    case _: ChangedState | _: ChangedMempool => () // do not touch the preserved registry
    case _: ChangedHistory | ContinueFullChainProbe | RetryFullChainProbe => ()
    case _: ScanOffChain | _: ScanOnChain | _: ScanInThePast | _: Rollback => ()
    case LockWallet => ()
    case _ => sender() ! Status.Failure(reason)
  }

}
