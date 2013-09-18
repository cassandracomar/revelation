from __future__ import print_function
import genc
import sys
import re

if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO

simple_types = {"int": "CInt",
                "float": "CFloat",
                "double": "CDouble",
                "char*": "CString",
                "char": "CChar",
                "size_t": "CSize"}


def toHSType(t):
    if t in simple_types:
        return simple_types[t]

    t = "<" + t + ">"
    t = re.sub(r"(.+)\*(>)", r"Ptr \1>", t)

    return t


class HSCWrapperGen(object):
    def __init__(self):
        self.hsc = StringIO()

    def gen_const(self, constinfo):
        if constinfo.isfractional:
            self.hsc.write("#fractional %s\n" % constinfo.cname,)
        else:
            self.hsc.write("#num %s\n" % constinfo.cname,)

    def gen_type(self, typeinfo):
        self.hsc.write("#starttype %s\n" % typeinfo.name)
        for field in typeinfo.fields:
            self.hsc.write("#field %s , %s\n"
                           % (field, toHSType(typeinfo.fields[field])))
        self.hsc.write("#stoptype\n")

    def gen_func(self, func):
        code = "#ccall %s , " % (func.get_wrapper_name(),)
        for a in func.args:
            code += "%s -> " % toHSType(a.tp)
        code = "IO %s\n" % toHSType(func.rettype)
        self.hsc.write(code)

    def gen(self, cgen):
        self.hsc.write("{-# LANGUAGE ForiegnFunctionInterface #-}\n")
        self.hsc.write("#include <bindings.dsl.h>\n")
        self.hsc.write("#include <opencv_generated.hpp>\n")
        self.hsc.write("module Bindings.RawBindings where\n")
        self.hsc.write("#strict_import\n")

        # Generate the code for consts, types, and functions
        for c in cgen.consts:
            self.gen_const(cgen.consts[c])
        for t in cgen.types:
            if not t in simple_types:
                self.gen_type(cgen.types[t])
        for f in cgen.funcs:
            self.gen_func(cgen.funcs[f])

        f = open(hscdstdir + "RawBindings.hsc", "wt")
        f.write(self.hsc.getvalue())
        f.close()

if __name__ == "__main__":
    header_dir = "/usr/local/include/"
    dstdir = "cbits/"
    hscdstdir = "Revelation/Bindings/"
    headers = None
    if len(sys.argv) > 1:
        header_dir = sys.argv[1]
    if len(sys.argv) > 2:
        dstdir = sys.argv[2]
    if len(sys.argv) > 3:
        headers = sys.argv[3:]

    cgen = genc.CWrapperGenerator()
    cgen.gen(header_dir, headers, dstdir)

    hsc = HSCWrapperGen()
    hsc.gen(cgen)
