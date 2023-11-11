package scorex.core.settings

import java.io.File
import java.net.InetSocketAddress
import com.typesafe.config.Config
import net.ceedubs.ficus.readers.ValueReader

trait SettingsReaders {
  implicit val fileReader: ValueReader[File] = (cfg, path) => new File(cfg.getString(path))
  implicit val byteValueReader: ValueReader[Byte] = (cfg, path) => cfg.getInt(path).toByte
  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = { (config: Config, path: String) =>
    val string = config.getString(path)
    val index = string.lastIndexOf(':')
    new InetSocketAddress(string.substring(0, index), string.substring(index + 1).toInt)
  }
}
