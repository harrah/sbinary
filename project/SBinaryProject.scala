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
		scalaVersion := "2.11.0-M6",
		crossVersion := CrossVersion.full,
		scalaXmlVersion := "1.0.0-RC6",
		scalaParserCombinatorsVersion := "1.0.0-RC4",
		scalaCheckVersion := "1.10.1",
		resolvers += Resolver.sonatypeRepo("snapshots"), // to allow compiling against snapshot versions of Scala
		includeTestDependencies <<= scalaVersion(_.startsWith("2.10."))
	)

	lazy val scalaXmlVersion = SettingKey[String]("scala-xml-version")
	lazy val scalaParserCombinatorsVersion = SettingKey[String]("scala-parser-combinators-version")
	lazy val scalaCheckVersion = SettingKey[String]("scalacheck-version")

	lazy val includeTestDependencies = SettingKey[Boolean]("include-test-dependencies")

	lazy val coreSettings = commonSettings ++ template ++ Seq(
		name := "SBinary",
		libraryDependencies ++= (
			if(includeTestDependencies.value) Seq(
				"org.scalacheck" %% "scalacheck" % scalaCheckVersion.value % "test" intransitive)
			else Nil ),
		libraryDependencies ++= (
			if(scalaVersion.value.startsWith("2.11.")) Seq(
				"org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion.value,
				"org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion.value)
			else Nil ),
		unmanagedResources in Compile <+= baseDirectory map { _ / "LICENSE" }
	)
	def aux(nameString: String) = commonSettings ++ Seq( publish := (), name := nameString )

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
		sourceGenerators <+= fmpp(x => x),
		fmpp <<= fmppTask,
		scalaSource <<= (baseDirectory, configuration) { (base,c) => base / (Defaults.prefix(c.name) + "src") },
		mappings in packageSrc <<= (managedSources, sourceManaged) map { (srcs, base) => srcs x relativeTo(base) },
		sources <<= managedSources(x => x)
	))
	lazy val fmppTask =
		(fullClasspath in fmppConfig, runner in fmpp, unmanagedSources, scalaSource, sourceManaged, fmppOptions, streams) map { (cp, r, sources, srcRoot, output, args, s) =>
			IO.delete(output)
			val arguments = "-U" +: "all" +: "-S" +: srcRoot.getAbsolutePath +: "-O" +: output.getAbsolutePath +: (args ++ sources.getPaths)
			toError(r.run("fmpp.tools.CommandLine", cp.files, arguments, s.log))
			(output ** "*.scala").get
		}
}
