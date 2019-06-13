import sbt._
import sbt.Keys._
import sbt.plugins.JvmPlugin

/** sets the build environment */
object BuildEnvPlugin extends AutoPlugin {

  // make sure it triggers automatically
  override def trigger: PluginTrigger = AllRequirements
  override def requires: JvmPlugin.type = JvmPlugin

  object autoImport {
    object BuildEnv extends Enumeration {
      val MainNet, TestNet, DevNet, Test = Value
    }

    val buildEnv = settingKey[BuildEnv.Value]("the current build environment")
  }
  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    buildEnv := {
      sys.props.get("env")
        .orElse(sys.env.get("BUILD_ENV"))
        .flatMap {
          case "mainnet" => Some(BuildEnv.MainNet)
          case "testnet" => Some(BuildEnv.TestNet)
          case "devnet" => Some(BuildEnv.DevNet)
          case "test" => Some(BuildEnv.Test)
          case _ => None
        }
        .getOrElse(BuildEnv.Test)
    },
    // give feed back
    onLoadMessage := {
      // depend on the old message as well
      val defaultMessage = onLoadMessage.value
      val env = buildEnv.value
      s"""|$defaultMessage
          |Running in build environment: $env""".stripMargin
    }
  )

}
