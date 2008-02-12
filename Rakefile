require 'jerbil'

SOURCE_DIR          = "src"
TEST_SOURCE_DIR     = "test-src"
BUILD_DIR           = "build"
DIST_DIR            = 'dist'
GENERATED_SRC       = 'generated-src'
FMPP                = 'tools/fmpp_0.9.13/bin/fmpp'
TEMPLATES           = 'templates'

SCALA_HOME          = ENV["SCALA_HOME"].gsub(/\\/, "/");
SCALA_LIBS          = FileList["#{SCALA_HOME}/lib/**/*.jar"]

LIB_DIR             = File.join(Rake.original_dir, "lib")
NATIVE_LIB_DIR      = File.join(LIB_DIR, "native")
TEST_LIB_DIR        = File.join(Rake.original_dir, "test-lib")

SOURCE_FILES        = FileList["#{SOURCE_DIR}/**/*.scala", "#{GENERATED_SRC}/**/*.scala"]
TEST_SOURCE_FILES   = FileList["#{TEST_SOURCE_DIR}/**/*.scala"]

CLASSPATH           = FileList["#{BUILD_DIR}", "#{LIB_DIR}/**/*.jar"]; 
TEST_CLASSPATH      = FileList["#{TEST_LIB_DIR}/**/*.jar"] + CLASSPATH

load_jvm(CLASSPATH + TEST_CLASSPATH, [])

task :default => :compile

task :generate do
  if (!File.exists?(GENERATED_SRC)) then
    mkdir_p GENERATED_SRC;
    sh "#{FMPP} -S #{TEMPLATES} -O #{GENERATED_SRC}"
  end
end

task :compile => :generate do
  mkdir_p "#{BUILD_DIR}" unless File.exists?(BUILD_DIR)
  sh "fsc -cp \"#{CLASSPATH.to_cp}\" -d #{BUILD_DIR} #{SOURCE_FILES}"
end

task :compiletests => :compile do
  sh "fsc -cp \"#{TEST_CLASSPATH.to_cp}\" -d #{BUILD_DIR} #{TEST_SOURCE_FILES}"
end

task :test => :compiletests do
  sh "scala -cp \"#{TEST_CLASSPATH.to_cp}\" sbinary.BinaryTests"
end

task :clean  do |t|
  rm_rf BUILD_DIR
  rm_rf GENERATED_SRC
  sh "fsc -shutdown"
end
