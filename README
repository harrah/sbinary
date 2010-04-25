SBinary is a library for describing binary protocols, in the form of mappings between Scala types and binary formats. It can be used as a robust serialization mechanism for Scala objects or a way of dealing with existing binary formats found in the wild.

It started out life as a loose port of Haskell's Data.Binary. It's since evolved a bit from there to take advantage of the features Scala implicits offer over Haskell type classes, but the core idea has remained the same.

SBinary was written by David MacIver.  You can find his introduction here: http://code.google.com/p/sbinary/wiki/IntroductionToSBinary.

= Getting SBinary =

SBinary is in the Scala Tools Repository.  If you are using sbt with Scala 2.7.7:
  val sbinary = "org.scala-tools.sbinary" %% "sbinary" % "0.3"

For Maven users, use "sbinary_2.7.7" for the artifact ID.


= Build instructions =

SBinary uses sbt to build.  Get sbt at http://code.google.com/p/simple-build-tool/wiki/Setup

To build and run the example:
sbt update publish-local 'project Binary Tree Example' run

This will retrieve dependencies and compile, package, and publish SBinary to your ~/.ivy2/local repository.   If you just want to use the jar directly, it is in the `target/` directory.

You can find the source for the example is in the examples/bt/ directory.