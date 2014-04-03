#!/usr/bin/env python2

import cgi
import pip
import pkg_resources
import sys, os, tarfile, zipfile, shutil, urllib2, getopt
from distutils.version import StrictVersion
from pip.util import splitext
from pip.exceptions import DistributionNotFound
from pip.index import PackageFinder
from pip.req import InstallRequirement
from pip.req import parse_requirements


#Check pip version before working
if StrictVersion(pip.__version__) < StrictVersion('1.4.1'):
    print("ERROR: Pip version should be 1.4.1 or more. Current version"
          " %s" % pip.__version__)
    exit(1)


class Bundler:
    def __init__(self, cache_directory, use_wheel=False):
        self.cache_directory = cache_directory
        self.local_finder = PackageFinder([self.cache_directory],
                                          [],
                                          use_wheel=use_wheel)
        self.external_finder = PackageFinder([],
                                             ["http://pypi.python.org/simple"],
                                             use_wheel=use_wheel)

    def _is_cached(self, file_name):
        return os.path.isfile(os.path.join(self.cache_directory, file_name))

    def _fetch_package(self,package):
        print(" -> downloading %s" % package.filename)
        remote_file = urllib2.urlopen(package.url)
        local_file = open(str(os.path.join(self.cache_directory,
                                           package.filename)), 'wb')
        local_file.write(remote_file.read())
        local_file.close()

    def _extract_dependencies(self, file_name):
        archive = None
        names = None
        dependencies = []

        if file_name.find('.tar.gz') > 0 or file_name.find('.tar.bz2') > 0:
            archive = tarfile.TarFile.open(os.path.join(self.cache_directory,
                                                        file_name))
        elif file_name.find('.zip') > 0:
            archive = zipfile.ZipFile(os.path.join(self.cache_directory,
                                                   file_name))

        if type(archive) is zipfile.ZipFile:
            names = archive.namelist()
        elif type(archive) is tarfile.TarFile:
            names = archive.getnames()

        if names is not None:
            package_requires = None
            package_name = str(splitext(file_name)[0]).strip()

            if os.path.join(package_name, "requirements.txt") in names:
                archive.extract(os.path.join(package_name, "requirements.txt"),
                                "tmp")
                package_requires = os.path.join("tmp", package_name,
                                                "requirements.txt")
            elif os.path.join(package_name, "tools/pip-requires") in names:
                archive.extract(os.path.join(package_name,
                                             "tools/pip-requires"), "tmp")
                package_requires = os.path.join("tmp", package_name,
                                                "tools/pip-requires")

            if package_requires is not None:
                install_reqs = parse_requirements(package_requires)
                dependencies = [str(ir.req) for ir in install_reqs]

            if os.path.isdir(os.path.join("tmp")):
                shutil.rmtree(os.path.join("tmp"))

        if archive is not None:
            archive.close()

        return dependencies

    def resolve(self, requires, parent=None, update=False):
        self.update = update

        if self.update:
            finder = self.external_finder
        else:
            finder = self.local_finder

        for require in requires:
            try:
                package = finder.find_requirement(
                    InstallRequirement.from_line(require, None), False)
                if not self._is_cached(package.filename):
                    if self.update:
                        self._fetch_package(package)
                    else:
                        print("-> %s not cached but needed" % package.filename)
                        exit(1)

                dependencies = self._extract_dependencies(package.filename)
                self.resolve(dependencies, update=self.update,
                             parent=package.filename)

            except DistributionNotFound as error:
                print(error)
                exit(1)


class Dir2Pi:
    def __init__(self, cache_directory):
        self.cache_directory = cache_directory

    def egg_to_package(self, file):
        """ Extracts the package name from an egg.
            egg_to_package("PyYAML-3.10-py2.7-macosx-10.7-x86_64.egg")
            ('PyYAML', '3.10-py2.7-macosx-10.7-x86_64.egg')
            egg_to_package("python_ldap-2.3.9-py2.7-macosx-10.3-fat.egg")
            ('python-ldap', '2.3.9-py2.7-macosx-10.3-fat.egg')
        """
        dist = pkg_resources.Distribution.from_location(None, file)
        name = dist.project_name
        return name, file[len(name)+1:]

    def file_to_package(self, file, basedir=None):
        """ Returns the package name for a given file.
            file_to_package("foo-1.2.3_rc1.tar.gz")
            ('foo', '1.2.3-rc1.tar.gz')
            file_to_package("foo-bar-1.2.tgz")
            ('foo-bar', '1.2.tgz')
        """
        if os.path.splitext(file)[1].lower() == ".egg":
            return self.egg_to_package(file)
        split = file.rsplit("-", 1)
        if len(split) != 2:
            msg = "unexpected file name: %s " % file
            msg += "(not in 'pkg-name-version.xxx' format"
            if basedir:
                msg += "; found in directory: %s" % basedir
            msg += ")"
            raise ValueError(msg)
        return split[0], pkg_resources.safe_name(split[1])

    def create_indexes(self):
        pkgdir = self.cache_directory
        if not os.path.isdir(pkgdir):
            raise ValueError("no such directory: %r" % (pkgdir, ))

        pkgdirpath = lambda *x: os.path.join(pkgdir, *x)

        if not os.path.isdir(pkgdirpath("simple")):
            return

        pkg_index = ("<html><head><title>Simple Index</title>"
                     "<meta name='api-version' value='2' /></head><body>\n")

        for file in os.listdir(pkgdir):
            pkg_filepath = os.path.join(pkgdir, file)
            if not os.path.isfile(pkg_filepath):
                continue
            pkg_basename = os.path.basename(file)
            if pkg_basename.startswith("."):
                continue 
            pkg_name, pkg_rest = self.file_to_package(pkg_basename, pkgdir)
            pkg_dir = pkgdirpath("simple", pkg_name)
            if not os.path.exists(pkg_dir):
                continue
            pkg_new_basename = "-".join([pkg_name, pkg_rest])
            pkg_name_html = cgi.escape(pkg_name)
            pkg_index += "<a href='{0}/'>{0}</a><br />\n".format(pkg_name_html)
            with open(os.path.join(pkg_dir, "index.html"), "a") as fp:
                pkg_new_basename_html = cgi.escape(pkg_new_basename)
                fp.write("<a href='%s'>%s</a><br />\n"
                         %(pkg_new_basename_html, pkg_new_basename_html))

        with open(pkgdirpath("simple/index.html"), "w") as fp:
            fp.write(pkg_index)


    def create_dir_simple(self):
        """
            Creates the directory self.cache_directory/simple/ and populates
            it with the directory structure required to use with
            pip's --index-url.

            Assumes that self.cache_directory contains a bunch of archives
            named
            'package-name-version.ext' (ex 'foo-2.1.tar.gz' or
            'foo-bar-1.3rc1.bz2').

            For example:

                $ ls packages/
                foo-1.2.tar.gz
                $ dir2pi packages/
                $ find packages/
                packages/
                packages/foo-1.2.tar.gz
                packages/simple/
                packages/simple/foo/
                packages/simple/foo/foo-1.2.tar.gz
        """
        pkgdir = self.cache_directory
        if not os.path.isdir(pkgdir):
            raise ValueError("no such directory: %r" % (pkgdir, ))
        
        pkgdirpath = lambda *x: os.path.join(pkgdir, *x)

        shutil.rmtree(pkgdirpath("simple"), ignore_errors=True)
        os.mkdir(pkgdirpath("simple"))

        for file in os.listdir(pkgdir):
            pkg_filepath = os.path.join(pkgdir, file)
            if not os.path.isfile(pkg_filepath):
                continue
            pkg_basename = os.path.basename(file)
            if pkg_basename.startswith("."):
                continue
            pkg_name, pkg_rest = self.file_to_package(pkg_basename, pkgdir)
            pkg_dir = pkgdirpath("simple", pkg_name)
            if not os.path.exists(pkg_dir):
                os.mkdir(pkg_dir)
            pkg_new_basename = "-".join([pkg_name, pkg_rest])
            symlink_target = os.path.join(pkg_dir, pkg_new_basename)
            if not os.path.exists(symlink_target):
                symlink_source = os.path.join("../../", pkg_basename)
                os.symlink(symlink_source, symlink_target)

def print_help():
    print 'pip-bundler.py <options> -c <cache directory> requires'
    print '\t-f\tfetch packages if not cached'


def main(argv):
    cache_directory = None
    cache_update = False
    try:
        opts, args = getopt.getopt(argv,"hfc:",["cache="])
    except getopt.GetoptError:
        print_help()
        exit(2)
    for opt, arg in opts:
        if opt == '-h':
            print_help()
            exit()
        elif opt in ("-f"):
            cache_update = True
        elif opt in ("-c"):
            cache_directory = arg

    if cache_directory is None:
        print("Cache directory not configured")
        exit(1)
    
    dir2pi = Dir2Pi(str(cache_directory))
    # Create indexes for each package. It needs for correct working of Bundler
    dir2pi.create_indexes()

    #Download packages include packages from requirements
    bundler = Bundler(str(cache_directory))
    bundler.resolve(args, update=cache_update)

    dir2pi.create_dir_simple()


if __name__ == "__main__":
    main(sys.argv[1:])
