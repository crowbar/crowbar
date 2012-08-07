#!/usr/bin/env ruby
#
# Generate initial .spec and _service files for each barclamp.
# This is most likely a one-off operation since we will probably
# want to manually tweak individual barclamp .spec files later.
#
# Arguments for having a spec file per barclamp:
#   - separate .spec    files are easier to read / edit / compare than one huge one
#   - separate _service files are easier to read / edit / compare than one huge one
#   - osc service disabledrun is much quicker and more fine-grained
#     (avoids unnecessary re-fetch of all barclamps)
#   - (re)builds of individual barclamps are much quicker and more fine-grained
#     (avoids unnecessary rebuild of all barclamps, and any consequent risk of
#      redundant package updates)
#   - separate changelog per barclamp
#
# Arguments for having a single spec file with a sub-package per barclamp:
#   - can't think of any right now

require 'pathname'
require 'erb'
require 'yaml'

def usage
  me = Pathname($0).basename
  puts <<EOUSAGE
Usage: #{me} GIT-PATH BUILD-SERVICE-PATH TEMPLATE-DIR BARCLAMP1 [BARCLAMP2 ...]

GIT-PATH is the path to the top-level crowbar git repository.

BUILD-SERVICE-PATH is the path to the OBS project where the
new barclamp package directories should be created.

TEMPLATE-DIR is the path where the .spec and _service ERB templates live.
EOUSAGE
  exit 0
end

def main
  usage if ARGV[0] =~ /^(-h|--help)$/ or ARGV.count < 4

  git_path, build_service_path, template_dir, *barclamps = ARGV

  [git_path, build_service_path].each do |path|
    raise "#{path} does not exist" unless File.directory? path
  end

  template_dir = Pathname(template_dir)
  spec_tmpl    = ERB.new(File.read(template_dir + 'crowbar-barclamp.spec.erb'))
  service_tmpl = ERB.new(File.read(template_dir + 'barclamp_service.erb'))

  barclamps.each do |bcname|
    bc = Barclamp.new(git_path, bcname)
    destdir = Pathname(build_service_path) + bc.pkg
    ensure_dir_exists(destdir)
    bc.gen_file_unless_exists(destdir + "#{bc.pkg}.spec", spec_tmpl)
    bc.gen_file_unless_exists(destdir + "_service",    service_tmpl)
  end
end

def ensure_dir_exists(dir)
  unless File.directory?(dir)
    Dir.mkdir(dir)
    puts "mkdir #{dir}"
  end
end

class Barclamp
  attr_reader :name, :displayname, :requires, :pkg

  def initialize(git_path, name)
    yaml_path = Pathname(git_path) + 'barclamps' + name + 'crowbar.yml'
    yaml = YAML.load_file(yaml_path)

    bcy = yaml['barclamp']

    @name = bcy['name']
    raise "name mismatch: #{name} != #{@name}" unless name == @name

    @displayname = bcy['display']
    @requires = (bcy['requires'] || []).inject(['crowbar']) { |reqs, req|
      reqs.push "crowbar-barclamp-#{req}" unless req == '@crowbar'
      reqs
    }.join ' '

    @pkg = "crowbar-barclamp-#@name"
  end

  def gen_file_unless_exists(outfile, tmpl)
    if File.exists?(outfile)
      puts "Won't overwrite existing #{outfile}"
    else
      gen_file(outfile, tmpl)
    end
  end

  def gen_file(outfile, tmpl)
    File.open(outfile.to_s, 'w') do |f|
      f.puts tmpl.result(binding)
    end
    puts "Wrote #{outfile}"
  end
end

main
