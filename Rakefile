require 'jerbil'

FMPP                = 'tools/fmpp_0.9.13/bin/fmpp'
TEMPLATES           = 'templates'

SCALA_HOME          = ENV["SCALA_HOME"].gsub(/\\/, "/");
SCALA_LIBS          = FileList["#{SCALA_HOME}/lib/**/*.jar"]

LIB_DIR             = File.join(Rake.original_dir, "lib")
NATIVE_LIB_DIR      = File.join(LIB_DIR, "native")
TEST_LIB_DIR        = File.join(Rake.original_dir, "test-lib")

CLASSPATH           = FileList["build", "#{LIB_DIR}/**/*.jar"]; 
TEST_CLASSPATH      = FileList["#{TEST_LIB_DIR}/**/*.jar"] + CLASSPATH

load_jvm(CLASSPATH + TEST_CLASSPATH, [])

task :default => :compile

task :compile do
  mkdir_p "build" unless File.exists? "build"
  mkdir_p "generated" unless File.exists? "generated"
  sh "#{FMPP} --ignore-temporary-files -O generated #{FileList["src/**/*.scala"]}"
  sh "fsc -cp \"#{CLASSPATH.to_cp}\" -d build #{FileList["generated/src/**/*.scala"]}"
end

task :compiletests => :compile do
  sh "#{FMPP} --ignore-temporary-files -O generated #{FileList["test-src/**/*.scala"]}"
  sh "fsc -cp \"#{TEST_CLASSPATH.to_cp}\" -d build #{FileList["generated/test-src/**/*.scala"]}"
end

task :test => :compiletests do
  sh "scala -cp \"#{TEST_CLASSPATH.to_cp}\" sbinary.BinaryTests"
end

task :clean  do |t|
  rm_rf "build"
  rm_rf "generated-src" 
  rm_rf "generated-test-src" 
  sh "fsc -shutdown"
end
