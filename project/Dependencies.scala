import sbt.*
import sbt.Keys.*

/** External library dependencies. */
object Dependencies:

  /** The version to use for each dependency. */
  object V:

    val tapir           = "1.13.25"
    val circe           = "0.14.16"
    val cats            = "2.13.0"
    val slick           = "3.6.1"
    val h2              = "2.3.232"
    val scalajs         = "2.8.1"
    val laminar         = "17.0.0"
    val laminext        = "0.17.0"
    val munit           = "1.2.4"

  /** Library dependencies associated with Tapir, for defining API endpoints. */
  lazy val tapir = libraryDependencies ++= Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-core"       % V.tapir,
    "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % V.tapir,
  )

  /** Library dependencies associated with Circe, for JSON parsing. */
  lazy val circe = libraryDependencies ++= Seq(
    "io.circe" %% "circe-core"   % V.circe,
    "io.circe" %% "circe-parser" % V.circe,
  )

  /** Library dependencies associated with cats, for FP abstractions. */
  lazy val cats = libraryDependencies ++=
    Seq("org.typelevel" %% "cats-core" % V.cats)

  /**
    * Library dependencies for database access, using
    * [Slick](https://scala-slick.org/) over whichever JDBC profile the host
    * application chooses. [H2](https://www.h2database.com/) is for tests alone.
    */
  lazy val database = libraryDependencies ++= Seq(
    "com.typesafe.slick" %% "slick" % V.slick,
    "com.h2database"      % "h2"    % V.h2 % Test,
  )

  /** Library dependencies associated with Scala.js, for JS interop. */
  lazy val scalajs = libraryDependencies ++=
    Seq("org.scala-js" %% "scalajs-dom" % V.scalajs)

  /** Library dependencies associated with Laminar, for client-side rendering. */
  lazy val laminar = libraryDependencies ++= Seq(
    "com.raquo"   %% "laminar"     % V.laminar,
    "io.laminext" %% "core"        % V.laminext,
    "io.laminext" %% "fetch"       % V.laminext,
    "io.laminext" %% "fetch-circe" % V.laminext,
  )

  /** Library dependencies for testing with MUnit. */
  lazy val munit = libraryDependencies ++=
    Seq("org.scalameta" %% "munit" % V.munit % Test)
