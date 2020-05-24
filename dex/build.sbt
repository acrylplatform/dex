import com.typesafe.sbt.packager.debian.DebianPlugin.autoImport.DebianConstants._

enablePlugins(RunApplicationSettings, ExtensionPackaging, GitVersioning)

resolvers += "dnvriend" at "http://dl.bintray.com/dnvriend/maven"
libraryDependencies ++= Dependencies.dex

val packageSettings = Seq(
  maintainer := "acrylplatform.com",
  packageSummary := "DEX",
  packageDescription := s"Decentralized EXchange for Acryl network. Compatible with ${nodeVersion.value} node version"
)

packageSettings
inScope(Global)(packageSettings)

lazy val versionSourceTask = Def.task {
  // WARNING!!!
  // Please, update the fallback version every major and minor releases.
  // This version is used then building from sources without Git repository
  // In case of not updating the version nodes build from headless sources will fail to connect to newer versions
  val FallbackVersion = (1, 0, 4)

  val versionFile      = sourceManaged.value / "com" / "acrylplatform" / "dex" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
  val (major, minor, patch) = version.value match {
    case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
    case x                            => FallbackVersion
  }

  IO.write(
    versionFile,
    s"""package com.acrylplatform.dex
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $patch)
       |}
       |""".stripMargin
  )
  Seq(versionFile)
}

inConfig(Compile)(Seq(sourceGenerators += versionSourceTask))

Debian / maintainerScripts := maintainerScriptsAppend((Debian / maintainerScripts).value - Postrm)(
  Postrm ->
    s"""#!/bin/sh
       |set -e
       |if [ "$$1" = purge ]; then
       |  rm -rf /var/lib/${nodePackageName.value}/matcher
       |fi""".stripMargin
)
