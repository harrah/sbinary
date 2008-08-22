require 'jerbil'

TEMPLATES           = 'templates'

SCALA_HOME          = ENV["SCALA_HOME"].gsub(/\\/, "/");
SCALA_LIBS          = FileList["#{SCALA_HOME}/lib/**/*.jar"]

LIB_DIR             = File.join(Rake.original_dir, "lib")
NATIVE_LIB_DIR      = File.join(LIB_DIR, "native")
TEST_LIB_DIR        = File.join(Rake.original_dir, "test-lib")

CLASSPATH           = FileList["build", "#{LIB_DIR}/**/*.jar"]; 
TEST_CLASSPATH      = FileList["#{TEST_LIB_DIR}/**/*.jar"] + CLASSPATH

SBINARY_VERSION     = "0.2"

JAR_FILE            = "sbinary-#{SBINARY_VERSION}.jar"

load_jvm(CLASSPATH + TEST_CLASSPATH, [])

task :default => :compile

task :generate do
  mkdir_p "generated" unless File.exists? "generated"
  sh "fmpp --ignore-temporary-files -O generated #{FileList["src/**/*.scala"]}"
end

task :compile => :generate do
  mkdir_p "build" unless File.exists? "build"
  sh "fsc -cp \"#{CLASSPATH.to_cp}\" -d build #{FileList["generated/src/**/*.scala"]}"
end

task :dist => [:clean, :compile] do
  mkdir_p "dist" 
  sh "jar -cf #{JAR_FILE} -C build ." 
  sh "mv #{JAR_FILE} dist"
end

task :compiletests do 
  sh "fmpp --ignore-temporary-files -O generated #{FileList["test-src/**/*.scala"]}"
  sh "fsc -cp \"#{TEST_CLASSPATH.to_cp}\" -d build #{FileList["generated/test-src/**/*.scala"]}"
end

task :runtests do
  sh "scala -cp \"#{TEST_CLASSPATH.to_cp}\" #{ENV["TEST"] || "sbinary.BinaryTests"}"
end

task :test => [:compiletests, :runtests]

task :doc => :generate do 
  mkdir_p "doc" unless File.exists? "doc"
  sh "scaladoc -d doc #{FileList["generated/src/**/*.scala"]}" 
end 

task :clean  do |t|
  sh "fsc -shutdown"
  rm_rf "build"
  rm_rf "generated" 
  rm_rf "dist"
  rm_rf "doc"  
end
