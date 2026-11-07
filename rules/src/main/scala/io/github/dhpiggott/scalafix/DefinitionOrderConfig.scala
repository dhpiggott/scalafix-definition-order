package io.github.dhpiggott.scalafix

import metaconfig.ConfDecoder
import metaconfig.generic.Surface

final case class DefinitionOrderConfig(exclude: List[String] = Nil)

object DefinitionOrderConfig {
  val default: DefinitionOrderConfig = DefinitionOrderConfig()

  // `surface` is metaconfig's description of the config's fields (their
  // names, types and defaults), which `deriveDecoder` reads
  // `DefinitionOrder.exclude` by. The macro takes it as an implicit argument
  // that SemanticDB doesn't record, so the rule can't see that `decoder` uses
  // it, and would put it below.
  // scalafix:off DefinitionOrder
  implicit val surface: Surface[DefinitionOrderConfig] =
    metaconfig.generic.deriveSurface[DefinitionOrderConfig]

  implicit val decoder: ConfDecoder[DefinitionOrderConfig] =
    metaconfig.generic.deriveDecoder(default)
  // scalafix:on DefinitionOrder
}
