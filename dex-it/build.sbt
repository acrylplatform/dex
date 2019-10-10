import AcrylExtensionDockerPlugin.autoImport._

enablePlugins(AcrylExtensionDockerPlugin, ItTestPlugin)

description := "DEX integration tests"
libraryDependencies ++= Dependencies.itTest

docker := docker.dependsOn(buildNodeContainer).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.acrylplatform/dex-it")),
    exposedPorts := Set(6886),
    additionalFiles ++= Seq(
      (LocalProject("dex") / Universal / stage).value,
      (Test / resourceDirectory).value / "template.conf",
      (Test / sourceDirectory).value / "container" / "wallet"
    )
  ))
