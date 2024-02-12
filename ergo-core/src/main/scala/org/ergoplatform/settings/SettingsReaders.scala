package org.ergoplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.readers.ValueReader

import java.io.File
import java.net.InetSocketAddress

trait SettingsReaders {
  implicit val fileReader: ValueReader[File] = {
    new ValueReader[File] {
      def read(cfg: Config, path: String) = new File(cfg.getString(path))
    }
  }

  implicit val byteValueReader: ValueReader[Byte] = {
    new ValueReader[Byte] {
      def read(cfg: Config, path: String) = cfg.getInt(path).toByte
    }
  }

  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = {
    new ValueReader[InetSocketAddress] {
      def read(config: Config, path: String) = {
        val string = config.getString(path)
        val index = string.lastIndexOf(':')
        new InetSocketAddress(string.substring(0, index), string.substring(index + 1).toInt)
      }
    }
  }
}
