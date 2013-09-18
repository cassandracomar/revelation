from __future__ import print_function
import genc
import sys

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


class HSCWrapperGen(object):
    def __init__(self):
        self.hsc = StringIO()

    def gen_const(self, const):
        if const.isfractional:
            self.write("#fractional %s\n" % const.name,)
        else:
            self.write("#num %s\n" % const.name,)

    def gen_type(self, type):
        self.write("#starttype %s\n" % type.cname)
        for field in type.fields:
            self.write("#field %s , %s\n" % (field, type.fields[field]))
        self.write("#stoptype\n")

    def gen_func(self, func):
        code = "#ccall %s , " % func.get_wrapper_name(),

        for a in func.args:
            code += "<%s> -> " % a.tp

        code = "IO <%s>\n" % func.rettype

    def gen(self, cgen):
        # Generate the code for consts, types, and functions
        for c in cgen.consts:
            self.gen_const(c, cgen.consts[c])
        for t in cgen.types:
            self.gen_type(t, cgen.types[t])
        for f in cgen.funcs:
            self.gen_func(f, cgen.funcs[f])

if __name__ == "__main__":
    header_dir = sys.argv[1]
    if len(sys.argv) > 2:
        dstdir = sys.argv[2]
    if len(sys.argv) > 3:
        headers = sys.argv[3:]

    cgen = genc.CWrapperGenerator()
    cgen.gen(header_dir, headers, dstdir)
