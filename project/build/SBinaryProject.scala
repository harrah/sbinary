import sbt._

class SBinaryProject(info: ProjectInfo) extends DefaultProject(info) with TemplateProject
{
	val sc =
		if(buildScalaVersion.startsWith("2.7")) "org.scala-tools.testing" %% "scalacheck" % "1.6" % "test"
		else "org.scala-tools.testing" % "scalacheck" % "1.7-SNAPSHOT" % "test" from("http://scalacheck.googlecode.com/files/scalacheck_2.8.0.Beta1-1.7-SNAPSHOT.jar")

	override def mainResources = super.mainResources +++ "LICENSE"

	// publishing
	override def managedStyle = ManagedStyle.Maven
	val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
	Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}
