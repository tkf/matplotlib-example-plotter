import os
import shutil
import fnmatch
from contextlib import contextmanager

import matplotlib
matplotlib.use("Agg")
from matplotlib import pyplot


def globtree(root, pattern):
    files = []

    def walker(pattern, dirname, fnames):
        files.extend(
            os.path.join(dirname, f) for f in fnmatch.filter(fnames, pattern))

    os.path.walk(root, walker, pattern)
    return files


def path_to_name(path, sep='-'):
    """Convert a `path` to a string without path separator."""
    sansext = os.path.splitext(path)[0]
    return sep.join(sansext.split(os.path.sep))


@contextmanager
def changedir(path):
    save = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(save)


def exportfigs(filename):
    dirname = os.path.dirname(filename)
    basename = os.path.basename(filename)

    print "Plotting {0}...".format(basename),
    with changedir(dirname):
        execfile(basename, {})
    print "Done"

    fignums = pyplot.get_fignums()
    figpaths = map("{0}{{0}}.pdf".format(dirname).format, fignums)
    for (i, path) in zip(fignums, figpaths):
        fig = pyplot.figure(i)
        fig.savefig(path)
    pyplot.close('all')
    return figpaths


def mkdirp(path):
    if not os.path.isdir(path):
        os.makedirs(path)


def mplegfigs(eg_dir, build_dir, ignore):
    files = [f for f in globtree(eg_dir, '*.py')
             if not any(fnmatch.fnmatch(f, p) for p in ignore)]
    names = [path_to_name(os.path.relpath(f, eg_dir)) for f in files]

    copies = []
    for (orig, na) in zip(files, names):
        basename = os.path.basename(orig)
        out_dir = os.path.join(build_dir, na)
        new_file = os.path.join(out_dir, basename)
        mkdirp(out_dir)
        shutil.copyfile(orig, new_file)
        copies.append(new_file)

    for new_file in copies:
        try:
            exportfigs(new_file)
        except (Exception, SystemExit) as e:
            print "Got error: {0}".format(e)


def main(args=None):
    from argparse import ArgumentParser
    parser = ArgumentParser(description=__doc__)
    parser.add_argument(
        '--eg-dir', default='matplotlib/examples')
    parser.add_argument(
        '--ignore', action='append',
        default=['*/user_interfaces/*', '*_qt*'])
    parser.add_argument(
        '--build-dir', default='build')
    ns = parser.parse_args(args)
    os.putenv("DISPLAY", "")    # Make sure window won't popup
    mplegfigs(**vars(ns))


if __name__ == '__main__':
    main()
