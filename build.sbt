name := "GCom"

version := "1.0"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers += "Twitter" at "http://maven.twttr.com/"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.2" 


libraryDependencies += "com.twitter"   % "util-collection"   % "5.3.10"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

// Add bogus test paths for java so generated eclipse project works
javaSource in Compile <<= baseDirectory(_ / "javasrc")

scalaSource in Test <<= baseDirectory(_ / "test")

//More bogus 
javaSource in Test <<= baseDirectory(_ / "javatest")


resourceDirectory in Compile <<= baseDirectory(_ / "src/resources")


resourceDirectory in Test <<= baseDirectory(_ / "test/resources")


