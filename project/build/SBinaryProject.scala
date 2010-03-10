import sbt._

class SBinaryProject(info: ProjectInfo) extends DefaultProject(info) with TemplateProject
{
	//val sc = "org.scala-tools.testing" %% "scalacheck" % "1.7-SNAPSHOT" % "test"

	override def mainResources = super.mainResources +++ "LICENSE"

	// publishing
	override def managedStyle = ManagedStyle.Maven
	val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
	Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}
