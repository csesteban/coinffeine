resolvers in ThisBuild ++= Seq(
  "coinffeine-releases" at "http://repository.coinffeine.com/nexus/content/repositories/releases/",
  "sonatype-releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/",
  Resolver.sonatypeRepo("public"),
  Classpaths.sbtPluginReleases
)

addSbtPlugin("com.github.gseitz" % "sbt-protobuf" % "0.3.3")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.8.0")

// We are using a custom build because 0.6.1 doesn't use the right version on the packaged
// artifacts
addSbtPlugin("no.vedaadata" % "sbt-javafx" % "0.6.2-coinffeine")

addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "1.3.0")

addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.0.2")

addSbtPlugin("com.ebiznext.sbt.plugins" % "sbt-cxf-wsdl2java" % "0.1.4")

addSbtPlugin("io.gatling" % "sbt-plugin" % "1.0")
