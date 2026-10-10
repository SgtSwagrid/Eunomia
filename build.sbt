import IdeSettings.packagePrefix
import org.scalajs.sbtplugin.ScalaJSPlugin
import sbt._
import sbt.Keys._
import sbtunidoc.BaseUnidocPlugin.autoImport.*
import sbtunidoc.ScalaUnidocPlugin

// Developed within a larger private project, which includes this build by
// reference and syncs it here. Every project id is prefixed with the library's
// name, so that none clashes with the host's.

val scala3 = "3.9.0"

ThisBuild / scalaVersion := scala3

ThisBuild / scalacOptions ++= Seq(
  "-explain",
  "-explain-types",
  "-explain-cyclic",
)

val projectRoot = "com.alecdorrington.eunomia"

lazy val eunomiaCore = projectMatrix
  .in(file("core"))
  .settings(
    name          := "eunomia-core",
    packagePrefix := projectRoot,

    // A matrix resolves its sources against the working directory, which is
    // not this build's own when a host includes it by reference.
    sourceDirectory := (ThisBuild / baseDirectory).value / "core" / "src",
    Dependencies.circe,
    Dependencies.cats,
    Dependencies.munit,
  )
  .jvmPlatform(scalaVersions = Seq(scala3))
  .jsPlatform(scalaVersions = Seq(scala3))

lazy val eunomiaTapir = projectMatrix
  .in(file("tapir"))
  .dependsOn(eunomiaCore)
  .settings(
    name          := "eunomia-tapir",
    packagePrefix := s"$projectRoot.tapir",

    // Pinned to this build's own base, as for the core.
    sourceDirectory := (ThisBuild / baseDirectory).value / "tapir" / "src",
    Dependencies.tapir,
    Dependencies.munit,
  )
  // Only the JVM's tests serve an endpoint, in memory.
  .jvmPlatform(
    scalaVersions = Seq(scala3),
    axisValues = Nil,
    configure = _.settings(Dependencies.tapirStub),
  )
  .jsPlatform(scalaVersions = Seq(scala3))

lazy val eunomiaServer = project
  .in(file("server"))
  .dependsOn(eunomiaCore.jvm(scala3))
  .settings(
    name          := "eunomia-server",
    packagePrefix := s"$projectRoot.server",
    Dependencies.database,
    Dependencies.munit,
  )

lazy val eunomiaClient = project
  .in(file("client"))
  .dependsOn(eunomiaCore.js(scala3))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name          := "eunomia-client",
    packagePrefix := s"$projectRoot.client",
    Dependencies.scalajs,
    Dependencies.laminar,
    Dependencies.circe,
    Dependencies.munit,
  )

lazy val eunomia = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(
    (eunomiaCore.projectRefs ++ eunomiaTapir.projectRefs ++
      Seq[ProjectReference](eunomiaServer, eunomiaClient)) *,
  )
  .settings(
    publish / skip := true,

    // Scaladoc comes from the JVM side alone, as the JS side would document
    // the shared sources twice.
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      eunomiaCore.jvm(scala3),
      eunomiaTapir.jvm(scala3),
      eunomiaServer,
    ),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-project", "Eunomia"),
  )
