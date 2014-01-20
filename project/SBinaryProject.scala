	import sbt._
	import Keys._

object SBinaryProject extends Build
{
	lazy val root = Project("root", file(".")) settings( aux("SBinary Parent") : _*) aggregate(core, treeExample)
	lazy val core = Project("core", file("core")) settings(coreSettings : _*)
	lazy val treeExample = Project("examples", file("examples") / "bt") settings( aux("SBinary Tree Example") : _*) dependsOn(core)

	lazy val commonSettings: Seq[Setting[_]] = Seq(
		organization := "org.scala-tools.sbinary",
		version := "0.4.3-SNAPSHOT",
		scalaVersion := "2.10.2",
		includeTestDependencies <<= scalaVersion(sv => sv.startsWith("2.10.") || sv.startsWith("2.11"))
	)

	lazy val includeTestDependencies = SettingKey[Boolean]("include-test-dependencies")
	lazy val scalaCheck = libraryDependencies <++= includeTestDependencies( incl =>
		if(incl)
			List("org.scalacheck" %% "scalacheck" % "1.11.1" % "test")
		else
			Nil
	)
	lazy val coreSettings = commonSettings ++ template ++ Seq(
		name := "SBinary",
		scalaCheck,
		libraryDependencies <++= scalaVersion(scalaXmlDep),
		unmanagedResources in Compile <+= baseDirectory map { _ / "LICENSE" }
	)
	def aux(nameString: String) = commonSettings ++ Seq( publish := (), name := nameString )

	def scalaXmlDep(scalaV: String): List[ModuleID] =
		if(scalaV.startsWith("2.11.")) List("org.scala-lang.modules" %% "scala-xml" % "1.0.0-RC7") else Nil

	/*** Templating **/

	lazy val fmpp = TaskKey[Seq[File]]("fmpp")
	lazy val fmppOptions = SettingKey[Seq[String]]("fmpp-options")
	lazy val fmppConfig = config("fmpp") hide

	lazy val template = fmppConfig(Test) ++ fmppConfig(Compile) ++ templateBase
	lazy val templateBase = Seq(
		libraryDependencies += "net.sourceforge.fmpp" % "fmpp" % "0.9.14" % fmppConfig.name,
		ivyConfigurations += fmppConfig,
		fmppOptions := "--ignore-temporary-files" :: Nil,
		fullClasspath in fmppConfig <<= update map { _ select configurationFilter(fmppConfig.name) map Attributed.blank }
	)
		
	def fmppConfig(c: Configuration): Seq[Setting[_]] = inConfig(c)(Seq(
		sourceGenerators <+= fmpp,
		fmpp <<= fmppTask,
		scalaSource <<= (baseDirectory, configuration) { (base,c) => base / (Defaults.prefix(c.name) + "src") },
		mappings in packageSrc <<= (managedSources, sourceManaged) map { (srcs, base) => srcs x relativeTo(base) },
		sources <<= managedSources
	))
	lazy val fmppTask =
		(fullClasspath in fmppConfig, runner in fmpp, unmanagedSources, scalaSource, sourceManaged, fmppOptions, streams) map { (cp, r, sources, srcRoot, output, args, s) =>
			IO.delete(output)
			val arguments = "-U" +: "all" +: "-S" +: srcRoot.getAbsolutePath +: "-O" +: output.getAbsolutePath +: (args ++ sources.getPaths)
			toError(r.run("fmpp.tools.CommandLine", cp.files, arguments, s.log))
			(output ** "*.scala").get
		}
}
