package scorex.core.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.consensus.History
import scorex.core.serialization.SerializerRegistry
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexEncoding
import scorex.core.PersistentNodeViewModifier
import scorex.util.{ModifierId, bytesToId}

import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

