TEMPLATES           = 'templates'

LIB_DIR             = File.join(Rake.original_dir, "lib")
NATIVE_LIB_DIR      = File.join(LIB_DIR, "native")
TEST_LIB_DIR        = File.join(Rake.original_dir, "test-lib")

CLASSPATH           = FileList["build", "#{LIB_DIR}/**/*.jar"]; 
TEST_CLASSPATH      = FileList["#{TEST_LIB_DIR}/**/*.jar"] + CLASSPATH

CLASSPATH_STRING    = CLASSPATH.join(":");
TEST_CLASSPATH_STRING    = TEST_CLASSPATH.join(":");

SBINARY_VERSION     = "0.3-alpha"

JAR_FILE            = "sbinary-#{SBINARY_VERSION}.jar"

task :default => :compile

task :generate do
  mkdir_p "generated" unless File.exists? "generated"
  sh "fmpp --ignore-temporary-files -O generated #{FileList["src/**/*.scala"]}"
end

task :compile => :generate do
  mkdir_p "build" unless File.exists? "build"
  sh "fsc -cp \"#{CLASSPATH_STRING}\" -d build #{FileList["generated/src/**/*.scala"]}"
end

task :dist => [:clean, :compile] do
  mkdir_p "dist" 
  sh "jar -cf #{JAR_FILE} -C build ." 
  sh "mv #{JAR_FILE} dist"
end

task :compiletests do 
  sh "fmpp --ignore-temporary-files -O generated #{FileList["test-src/**/*.scala"]}"
  sh "fsc -cp \"#{TEST_CLASSPATH_STRING}\" -d build #{FileList["generated/test-src/**/*.scala"]}"
end

task :runtests do
  sh "scala -cp \"#{TEST_CLASSPATH_STRING}\" #{ENV["TEST"] || "sbinary.FormatTests"}"
end

task :shell do
  sh "scala -cp \"#{TEST_CLASSPATH_STRING}\""
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
