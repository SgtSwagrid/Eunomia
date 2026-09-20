import IdeSettings.packagePrefix
import org.scalajs.sbtplugin.ScalaJSPlugin
import sbt._
import sbt.Keys._
import sbtunidoc.BaseUnidocPlugin.autoImport.*
import sbtunidoc.ScalaUnidocPlugin

// This build is developed as part of a larger private project,
// which includes it by reference and from which it is automatically synchronised.
// Every project is prefixed with the library's name, so that none clashes with a host's own.

val scala3 = "3.8.4"

ThisBuild / scalaVersion := scala3

ThisBuild / scalacOptions ++= Seq(
  "-explain",
  "-explain-types",
  "-explain-cyclic",
)

/** The base package prefix shared across all subprojects. */
val projectRoot = "com.alecdorrington.eunomia"

/**
  * The query model, the shared list schema and the API endpoint inputs, shared
  * by server and client. Cross-compiled for JVM and JS.
  */
lazy val eunomiaCore = projectMatrix
  .in(file("core"))
  .settings(
    name          := "eunomia-core",
    packagePrefix := projectRoot,

    // A matrix resolves its sources against the working directory, which is
    // not this build's own when a host includes it by reference:
    sourceDirectory := (ThisBuild / baseDirectory).value / "core" / "src",
    Dependencies.tapir,
    Dependencies.circe,
    Dependencies.cats,
    Dependencies.munit,
  )
  .jvmPlatform(scalaVersions = Seq(scala3))
  .jsPlatform(scalaVersions = Seq(scala3))

/**
  * The server half: running list queries in SQL, over whichever JDBC profile
  * the host application uses.
  */
lazy val eunomiaServer = project
  .in(file("server"))
  .dependsOn(eunomiaCore.jvm(scala3))
  .settings(
    name          := "eunomia-server",
    packagePrefix := s"$projectRoot.server",
    Dependencies.database,
    Dependencies.circe,
    Dependencies.cats,
    Dependencies.munit,
  )

/**
  * The client half: headless list state and an unstyled table, filtering in the
  * browser or on the server alike.
  */
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
    Dependencies.cats,
    Dependencies.munit,
  )

lazy val eunomia = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(
    (eunomiaCore.projectRefs ++
      Seq[ProjectReference](eunomiaServer, eunomiaClient)) *,
  )
  .settings(
    publish / skip := true,

    // Scaladoc is aggregated from the JVM side alone, as the JS side would
    // only document the shared sources a second time:
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(eunomiaCore.jvm(scala3), eunomiaServer),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-project", "Eunomia"),
  )
