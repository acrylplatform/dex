import CommonSettings.autoImport.network
import ReleasePlugin.autoImport._
import sbt.Keys._
import sbt._
import sbt.internal.inc.ReflectUtilities

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

def nodeVersionTag: String = "v1.0.3"

lazy val node = ProjectRef(uri(s"git://github.com/acrylplatform/Acryl.git#$nodeVersionTag"), "node")

lazy val `node-it` = ProjectRef(uri(s"git://github.com/acrylplatform/Acryl.git#$nodeVersionTag"), "node-it")

lazy val dex = project.dependsOn(node % "compile;test->test;runtime->provided")

lazy val `dex-it` = project
  .dependsOn(
    dex,
    `node-it` % "compile;test->test"
  )

lazy val `dex-generator` = project.dependsOn(
  dex,
  `node-it` % "compile->test", // Without this IDEA doesn't find classes
  `dex-it`  % "compile->test"
)

lazy val it = project
  .settings(
    description := "Hack for near future to support builds in TeamCity for old and new branches both",
    Test / test := Def
      .sequential(
        root / Compile / packageAll,
        `dex-it` / Docker / docker,
        `dex-it` / Test / test
      )
      .value
  )

lazy val root = (project in file("."))
  .aggregate(
    dex,
    `dex-it`,
    `dex-generator`
  )

inScope(Global)(
  Seq(
    scalaVersion := "2.12.8",
    organization := "com.acrylplatform",
    organizationName := "Acryl Platform",
    organizationHomepage := Some(url("https://acrylplatform.com")),
    scmInfo := Some(ScmInfo(url("https://github.com/acrylplatform/dex"), "git@github.com:acrylplatform/dex.git", None)),
    licenses := Seq(("MIT", url("https://github.com/acrylplatform/dex/blob/master/LICENSE"))),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-Ywarn-unused:-implicits",
      "-Xlint",
      "-Ypartial-unification",
      "-opt:l:inline",
      "-opt-inline-from:**"
    ),
    crossPaths := false,
    scalafmtOnCompile := false,
    dependencyOverrides ++= Dependencies.enforcedVersions.value,
    cancelable := true,
    logBuffered := false,
    coverageExcludedPackages := ".*",
    parallelExecution := false,
    testListeners := Seq.empty, // Fix for doubled test reports
    /* http://www.scalatest.org/user_guide/using_the_runner
     * o - select the standard output reporter
     * I - show reminder of failed and canceled tests without stack traces
     * D - show all durations
     * O - drop InfoProvided events
     * F - show full stack traces
     * u - select the JUnit XML reporter with output directory
     */
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true"),
    concurrentRestrictions := {
      val threadNumber = Option(System.getenv("SBT_THREAD_NUMBER")).fold(1)(_.toInt)
      Seq(Tags.limit(Tags.ForkedTestGroup, threadNumber))
    },
    network := NodeNetwork(sys.props.get("network")),
    nodeVersion := (node / version).value,
    buildNodeContainer := (`node-it` / Docker / docker).value
  ))

// ThisBuild options
git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")

// root project settings
enablePlugins(ReleasePlugin)

// https://stackoverflow.com/a/48592704/4050580
def allProjects: List[ProjectReference] = ReflectUtilities.allVals[Project](this).values.toList.map(x => x: ProjectReference) ++ List(
  node,
  `node-it`
)

Compile / cleanAll := {
  val xs = allProjects
  streams.value.log.info(s"Cleaning ${xs.mkString(", ")}")
  clean.all(ScopeFilter(inProjects(allProjects: _*), inConfigurations(Compile, Test))).value
}

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw := {
  // try/finally is a hack to run clean before all tasks
  try {
    (root / Compile / cleanAll).value
  } finally {
    (dex / Test / test).value
    (`dex-generator` / Test / compile).value
  }
}

commands += Command.command("checkPR") { state =>
  val updatedState = Project
    .extract(state)
    .appendWithoutSession(Seq(Global / scalacOptions ++= Seq("-Xfatal-warnings", "-Ywarn-unused:-imports")), state)
  Project.extract(updatedState).runTask(root / checkPRRaw, updatedState)
  state
}

// IDE settings
ideExcludedDirectories := Seq((root / baseDirectory).value / "_local")
