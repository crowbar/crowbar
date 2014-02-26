#!/usr/bin/env python2

import sys, os, tarfile, zipfile, shutil, urllib2, getopt
from pip.util import splitext
from pip.exceptions import DistributionNotFound
from pip.index import PackageFinder
from pip.req import InstallRequirement

class Bundler:
    def __init__(self, cache_directory, use_wheel=False):
        self.cache_directory = cache_directory
        self.finder = PackageFinder([], ["http://pypi.python.org/simple"], use_wheel=use_wheel)

    def _is_cached(self, file_name):
        return os.path.isfile(os.path.join(self.cache_directory, file_name))

    def _fetch_package(self,package):
        print(" -> downloading %s" % package.filename)
        remote_file = urllib2.urlopen(package.url)
        local_file = open(str(os.path.join(self.cache_directory, package.filename)), 'wb')
        local_file.write(remote_file.read())
        local_file.close()

    def _extract_dependencies(self, file_name):
        archive = None
        names = None
        dependencies = []

        if file_name.find('.tar.gz') > 0 or file_name.find('.tar.bz2') > 0:
            archive = tarfile.TarFile.open(os.path.join(self.cache_directory, file_name))
        elif file_name.find('.zip') > 0:
            archive = zipfile.ZipFile(os.path.join(self.cache_directory, file_name))

        if type(archive) is zipfile.ZipFile:
            names = archive.namelist()
        elif type(archive) is tarfile.TarFile:
            names = archive.getnames()

        if names is not None:
            package_requires = None
            package_name = str(splitext(file_name)[0]).strip()

            if os.path.join(package_name, "requirements.txt") in names:
                archive.extract(os.path.join(package_name, "requirements.txt"), "tmp")
                package_requires = os.path.join("tmp", package_name, "requirements.txt")
            elif os.path.join(package_name, "tools/pip-requires") in names:
                archive.extract(os.path.join(package_name, "tools/pip-requires"), "tmp")
                package_requires = os.path.join("tmp", package_name, "tools/pip-requires")

            if package_requires is not None:
                for req in open(package_requires):
                    if len(req.strip()) is 0:
                        continue
                    dependencies.append(req.strip())


            if os.path.isdir(os.path.join("tmp")):
                shutil.rmtree(os.path.join("tmp"))

        if archive is not None:
            archive.close()

        return dependencies

    def resolve(self, requires, parent=None, update=False):
        self.update = update
        for require in requires:
            try:
                package = self.finder.find_requirement(InstallRequirement.from_line(require, None), False)
                if not self._is_cached(package.filename):
                    if self.update:
                        self._fetch_package(package)
                    else:
                        print(" -> %s not cached but needed" % package.filename)
                        exit(1)

                dependencies = self._extract_dependencies(package.filename)
                self.resolve(dependencies, update=self.update, parent=package.filename)

            except DistributionNotFound as error:
                print(error)
                exit(1)

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

    bundler = Bundler(str(cache_directory))

    bundler.resolve(args, update=cache_update)


if __name__ == "__main__":
    main(sys.argv[1:])
