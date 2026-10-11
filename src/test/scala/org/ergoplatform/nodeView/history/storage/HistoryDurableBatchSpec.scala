package org.ergoplatform.nodeView.history.storage

import java.io.IOException
import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method, Proxy}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.iq80.leveldb.{DB, WriteBatch, WriteOptions}
import scorex.db.LDBKVStore
import scala.util.control.ControlThrowable

class HistoryDurableBatchSpec extends ErgoCorePropertyTest with DBSpec {
  private def delegate(method: Method, target: AnyRef, arguments: Array[AnyRef]): AnyRef =
    try method.invoke(target, arguments: _*) catch {
      case error: InvocationTargetException => throw error.getCause
    }

  property("durable batch requests sync and atomically inserts and removes ordinary rows") {
    withDb { db =>
      var synced = false
      val decorated = Proxy.newProxyInstance(classOf[DB].getClassLoader, Array(classOf[DB]), new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
          if (method.getName == "write" && args.length == 2) synced = args(1).asInstanceOf[WriteOptions].sync()
          delegate(method, db, Option(args).getOrElse(Array.empty[AnyRef]))
        }
      }).asInstanceOf[DB]
      val store = new LDBKVStore(decorated)
      store.insert(Array[Byte](1), Array[Byte](2)).get
      store.updateDurable(Array(Array[Byte](3)), Array(Array[Byte](4)), Array(Array[Byte](1))).get
      synced shouldBe true
      store.get(Array[Byte](1)) shouldBe None
      store.get(Array[Byte](3)).get.toSeq shouldBe Seq[Byte](4)
    }
  }

  property("durable batch preserves the write failure and suppresses a cleanup failure") {
    withDb { db =>
      val writeError = new org.iq80.leveldb.DBException("classified durable batch write")
      val cleanupError = new IOException("classified batch cleanup")
      val realBatch = db.createWriteBatch()
      val batch = Proxy.newProxyInstance(classOf[WriteBatch].getClassLoader, Array(classOf[WriteBatch]), new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
          if (method.getName == "close") {
            realBatch.close()
            throw cleanupError
          }
          delegate(method, realBatch, Option(args).getOrElse(Array.empty[AnyRef]))
        }
      }).asInstanceOf[WriteBatch]
      val decorated = Proxy.newProxyInstance(classOf[DB].getClassLoader, Array(classOf[DB]), new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = method.getName match {
          case "createWriteBatch" => batch
          case "write" => throw writeError
          case _ => delegate(method, db, Option(args).getOrElse(Array.empty[AnyRef]))
        }
      }).asInstanceOf[DB]
      val error = new LDBKVStore(decorated).updateDurable(Array(Array[Byte](1)), Array(Array[Byte](2)), Array.empty).failed.get
      error shouldBe writeError
      error.getSuppressed.toSeq shouldBe Seq(cleanupError)
      db.get(Array[Byte](1)) shouldBe null
    }
  }

  property("durable batch reports cleanup failure even when the write completed") {
    withDb { db =>
      val cleanupError = new IOException("classified cleanup after durable write")
      val realBatch = db.createWriteBatch()
      val batch = Proxy.newProxyInstance(classOf[WriteBatch].getClassLoader, Array(classOf[WriteBatch]), new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
          if (method.getName == "close") {
            realBatch.close()
            throw cleanupError
          }
          delegate(method, realBatch, Option(args).getOrElse(Array.empty[AnyRef]))
        }
      }).asInstanceOf[WriteBatch]
      val decorated = Proxy.newProxyInstance(classOf[DB].getClassLoader, Array(classOf[DB]), new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = method.getName match {
          case "createWriteBatch" => batch
          case "write" => delegate(method, db, Array[AnyRef](realBatch, args(1)))
          case _ => delegate(method, db, Option(args).getOrElse(Array.empty[AnyRef]))
        }
      }).asInstanceOf[DB]
      val result = new LDBKVStore(decorated).updateDurable(Array(Array[Byte](1)), Array(Array[Byte](2)), Array.empty)
      result.failed.get shouldBe cleanupError
      db.get(Array[Byte](1)).toSeq shouldBe Seq[Byte](2)
    }
  }

  for {
    interrupted <- Seq(false, true)
    writeFails <- Seq(false, true)
  } {
    property(s"durable batch propagates cleanup control failure (interrupted=$interrupted, writeFails=$writeFails)") {
      withDb { db =>
        val control: Throwable = if (interrupted) new InterruptedException("controlled batch interruption")
          else new ControlThrowable {}
        val writeError = new org.iq80.leveldb.DBException("controlled write failure")
        val realBatch = db.createWriteBatch()
        // A direct wrapper preserves control throwables without Java Proxy exception wrapping.
        val batch = new WriteBatch {
          override def put(key: Array[Byte], value: Array[Byte]): WriteBatch = {
            realBatch.put(key, value)
            this
          }
          override def delete(key: Array[Byte]): WriteBatch = {
            realBatch.delete(key)
            this
          }
          override def close(): Unit = {
            realBatch.close()
            throw control
          }
        }
        val decorated = Proxy.newProxyInstance(classOf[DB].getClassLoader, Array(classOf[DB]), new InvocationHandler {
          override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = method.getName match {
            case "createWriteBatch" => batch
            case "write" if writeFails => throw writeError
            case "write" => delegate(method, db, Array[AnyRef](realBatch, args(1)))
            case _ => delegate(method, db, Option(args).getOrElse(Array.empty[AnyRef]))
          }
        }).asInstanceOf[DB]
        val observed = try {
          new LDBKVStore(decorated).updateDurable(Array(Array[Byte](1)), Array(Array[Byte](2)), Array.empty)
          fail("A control throwable must escape the durable batch")
        } catch {
          case error: Throwable => error
        }
        observed shouldBe control
      }
    }
  }
}
