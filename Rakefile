require 'rubygems'
require 'sysinfo'
require 'fileutils'

ARTIFACT="dist/build/spy/spy".freeze
DISTRIBUTION="./distribution".freeze

desc "Clean build artifacts"
task :clean do
  cabal 'clean'
end

desc "Create the spy binary"
task :build => [:setup, :clean, :compile]

task :setup do
  ["install --only-dependencies"].each do |cmd|
    cabal cmd
  end
end

task :compile do
  %w(configure build).each do |cmd|
    cabal cmd
  end
  `strip #{ARTIFACT}`
end

desc "Build and package the spy distribution"
task :package => [:build, :generate_man] do
  include FileUtils
  with_temp_dir "spy" do |package_dir|
    cp "README.md", package_dir
    %w(LICENSE).each {|f| cp f, package_dir }
    bin_dir = File.join(package_dir, "bin")
    mkdir bin_dir
    cp ARTIFACT, bin_dir
    cp_r "#{DISTRIBUTION}/man", package_dir
    cp_r "#{DISTRIBUTION}/Makefile", package_dir
    sysinfo = SysInfo.new
    `tar zfc spy-#{sysinfo.impl}-#{sysinfo.arch}-#{version}.tar.gz spy`
  end
end

desc "Run Hlint on the sources"
task :hlint => :build do
  print `hlint src -c --report --utf8`
end

desc "Rebuild and copy spy into the cabal bin directory"
task :rebuild => :build do
  cabal 'copy'
end

desc "Create man page"
task :generate_man do
  `pandoc -s -w man ./#{DISTRIBUTION}/man/spy.1.md -o ./#{DISTRIBUTION}/man/spy.1`
end

desc "Run the tests"
task :test do
  ["clean", "install --only-dependencies --enable-tests", "configure --enable-tests", "build", "test"].each do |cmd|
    cabal cmd
  end
end

desc "Update version to the next minor version, tag the result"
task :bump_minor_version do
  current_version = `git describe --tags`.strip.split("-").first.gsub(/^v/, '')
  major, minor = current_version.split(".").map {|e| e.to_i}
  next_version = [major, minor.next].join(".")

  # Update version in Main
  update_file(File.join("src", "Main.hs"), /version = "[^"]+"/, "version = \"spy v#{next_version}, (C) Stefan Saasen\"")

  # Update cabal file
  update_file("spy.cabal", /^version:.*$/, "version:            #{next_version}")

  # Update download link
  update_file("README.md", /spy-osx-x86_64-v(.*?).tar.gz/, "spy-osx-x86_64-v#{next_version}.tar.gz")

  # Commit and tag
  msg = "Bump version to #{next_version}"
  puts msg
  `git add -u`
  `git commit -m "#{msg}"`
  `git tag -a -m "#{msg}" v#{next_version}`
end

def version
  `git describe --tag --always`.strip
end

def cabal(cmd)
  print `cabal #{cmd}`
  raise "Failed to execute 'cabal #{cmd}'" unless $?.to_i.eql?(0)
end

def with_temp_dir(name)
  raise "The directory #{name} already exists" if File.directory?(name)
  mkdir name
  yield name
ensure
  rm_r name
end


def update_file(file, pattern, replace)
  updated = IO.read(file)
  File.open(file, "w") do |f|
    f << updated.gsub(pattern, replace)
  end
end

