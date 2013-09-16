from __future__ import print_function
import hdr_parser
import re
import sys

if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO

_types = ["CvANN"]                              # literally, underscore types.
namespaces = ["SimpleBlobDetector"]
empty_types = ["cvflann", "flann"]


class TypeInfo(object):
    _types = list(map(lambda t: re.compile(r"" + t + r"_(\w+)"), _types))
    ptr_type = re.compile(r"^Ptr_(\w+)$")
    generic_type = re.compile(r"(\w+?)_((\w{2,}))")
    nss = list(zip(namespaces, map(lambda ns: re.compile(r"" + ns + r"_(\w+)"),
                                   namespaces)))
    empty_types = list(map(lambda et: re.compile(r"" + et + r"_(\w+)"),
                           empty_types))

    def gen_cname(name):
        cname = name

        cname = TypeInfo.ptr_type.sub(r"\1*", cname)
        for m in TypeInfo.empty_types:
            cname = m.sub(r"\1", cname)

        for ns, m in TypeInfo.nss:
            cname = m.sub(r"" + ns + r"::\1", cname)

        while (TypeInfo.generic_type.search(cname) and
               not any(t.match(cname) for t in TypeInfo._types)):
            cname = TypeInfo.generic_type.sub(r"\1<\2>", cname)

        return cname


class ConstInfo(object):
    def __init__(self, name, val):
        self.cname = name.replace(".", "::")
        self.name = name.replace(".", "_")
        self.name = re.sub(r"cv_([a-z])([A-Z])", r"cv_\1_\2", self.name)
        self.name = self.name.upper()
        if self.name.startswith("CV") and self.name[2] != "_":
            self.name = "CV_" + self.name[2:]
        self.value = val


class ArgInfo(object):
    def __init__(self, arg_tuple):
        self.tp = arg_tuple[0]
        self.name = arg_tuple[1]
        self.defval = arg_tuple[2]
        self.isarray = False
        self.arraylen = 0
        self.arraycvt = None
        self.inputarg = True
        self.outputarg = False
        self.returnarg = False
        for m in arg_tuple[3]:
            if m == "/O":
                self.inputarg = False
                self.outputarg = True
                self.returnarg = True
            elif m == "/IO":
                self.inputarg = True
                self.outputarg = True
                self.returnarg = True
            elif m.startswith("/A"):
                self.isarray = True
                self.arraylen = m[2:].strip()
            elif m.startswith("/CA"):
                self.isarray = True
                self.arraycvt = m[2:].strip()
        self.py_inputarg = False
        self.py_outputarg = False

    def isbig(self):
        return self.tp == "Mat" or self.tp == "vector_Mat"

    def crepr(self):
        return "ArgInfo(\"%s\", %d)" % (self.name, self.outputarg)


def close(s):
    if s[-1] != "(":
        # strip trailing comma and space, and close the prototype
        return s[:-2] + ");"
    else:
        return s + ");"


class FuncInfo(object):
    def __init__(self, classname, name, cname,
                 rettype, isconstructor, ismethod, args):
        self.classname = classname
        self.name = name
        self.cname = cname
        self.isconstructor = isconstructor
        self.variants = []
        self.args = args
        self.rettype = rettype
        if ismethod:
            self_arg = ArgInfo((classname + "*", "self", None, []))
            self.args = [self_arg] + self.args
        self.ismethod = ismethod

    def get_wrapper_name(self):
        name = self.name
        if self.classname:
            classname = self.classname + "_"
            if "[" in name:
                name = "getelem"
        else:
            classname = ""
        return "cv_" + classname + name

    def get_wrapper_prototype(self):
        full_fname = self.get_wrapper_name()
        ret = self.classname + "*" if self.isconstructor else self.rettype
        proto = "%s %s(" % (TypeInfo.gen_cname(ret), full_fname)
        for arg in self.args:
            if arg.isarray:
                proto += "%s* %s, " % (TypeInfo.gen_cname(arg.tp), arg.name)
            else:
                proto += "%s %s, " % (TypeInfo.gen_cname(arg.tp), arg.name)

        return close(proto)

    def gen_code(self):
        proto = self.get_wrapper_prototype()[:-1]
        code = "%s {\n" % (proto,)

        ret = "" if self.rettype == "void" else "return "
        prefix = ""
        postfix = self.cname
        args = self.args
        if self.isconstructor:
            prefix = "new "
            postfix = self.classname
        elif self.ismethod:
            prefix = "self->"
            args = args[1:]

        call = prefix + "%s(" % (postfix,)

        for arg in args:
            call += arg.name + ", "

        code += "\t" + ret + close(call)
        code += "\n}\n"

        return code


class CWrapperGenerator(object):
    def __init__(self):
        self.clear()

    def clear(self):
        self.funcs = {}
        self.consts = {}
        self.source = StringIO()
        self.header = StringIO()

    def add_const(self, name, decl):
        constinfo = ConstInfo(name, decl[1])

        if constinfo.name in self.consts:
            print("Generator error: constant %s (cname=%s) already exists"
                  % (constinfo.name, constinfo.cname))
            sys.exit(-1)
        self.consts[constinfo.name] = constinfo

    def add_func(self, decl):
        classname = bareclassname = ""
        name = decl[0]           # looks like cv{.classname}*.func
        dpos = name.rfind(".")
        if dpos >= 0 and name[:dpos] != "cv":
            classname = bareclassname = re.sub(r"^cv\.", "", name[:dpos])
            name = name[dpos + 1:]
            dpos = classname.rfind(".")
            if dpos >= 0:
                bareclassname = classname[dpos + 1:]
                classname = classname.replace(".", "_")

        cname = name
        name = re.sub(r"^cv\.", "", name)
        isconstructor = cname == bareclassname
        ismethod = not isconstructor and bareclassname != ""

        cname = cname.replace(".", "::")

        args = list(map(ArgInfo, decl[3]))

        if name in self.funcs.keys():
            #overloaded function...
            name += str(len(args))

        self.funcs[name] = FuncInfo(bareclassname, name, cname,
                                    decl[1], isconstructor, ismethod, args)

    def save(self, path, name, buf):
        f = open(path + "/" + name, "wt")
        f.write(buf.getvalue())
        f.close()

    def gen_const_reg(self, constinfo):
        self.header.write("#define %s %s\n"
                          % (constinfo.name, constinfo.value))

    def prep_src(self):
        self.source.write("#include \"opencv_generated.hpp\"\n")
        self.source.write("using namespace cv;\n")
        self.source.write("using namespace std;\n")
        self.source.write("using namespace flann;\n")
        self.source.write("extern \"C\" {\n")

    def prep_header(self):
        self.header.write("using namespace cv;\n")
        self.header.write("using namespace std;\n")
        self.header.write("using namespace flann;\n")
        self.header.write("extern \"C\" {\n")

    def finalize_and_write(self, output_path):
        self.source.write("}")
        self.save(output_path, "opencv_generated.hpp", self.header)
        self.save(output_path, "opencv_generated.cpp", self.source)

    def readHeaders(self, srcfiles):
        parser = hdr_parser.CppHeaderParser()

        self.header.write("#include <vector>\n")
        for hdr in srcfiles:
            decls = parser.parse(hdr)
            for decl in decls:
                name = decl[0]
                if name.startswith("struct") or name.startswith("class"):
                    pass
                elif name.startswith("const"):
                    self.add_const(name.replace("const ", "").strip(), decl)
                else:
                    self.add_func(decl)
            self.header.write("#include \"" + hdr + "\"\n")

    def gen(self, srcfiles, output_path):
        self.clear()

        self.header.write("#include <opencv2/opencv.hpp>\n")
        self.readHeaders(srcfiles)
        self.prep_src()

        constlist = list(self.consts.items())
        constlist.sort()
        for name, const in constlist:
            self.gen_const_reg(const)

        self.prep_header()
        funclist = list(self.funcs.items())
        funclist.sort()
        for name, func in funclist:
            prototype = func.get_wrapper_prototype() + "\n"
            code = func.gen_code()
            self.header.write(prototype)
            self.source.write(code)

        self.header.write("}")
        self.source.write("}")

        self.finalize_and_write(output_path)


if __name__ == "__main__":
    srcfiles = hdr_parser.opencv_hdr_list
    dstdir = "."
    if len(sys.argv) > 1:
        dstdir = sys.argv[1]
    if len(sys.argv) > 2:
        srcfiles = sys.argv[2:]

    generator = CWrapperGenerator()
    generator.gen(srcfiles, dstdir)
