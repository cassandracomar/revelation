from __future__ import print_function
import hdr_parser, sys, re, os
from string import Template

if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO

gen_template_simple_cfunc_decl = Template("""
    ${ret} cv${name}()
""")

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
        return self.tp == "Mat" or self.tp == "vector_Mat"# or self.tp.startswith("vector")

    def crepr(self):
        return "ArgInfo(\"%s\", %d)" % (self.name, self.outputarg)

def close(s):
    if s[-1] != "(":
        return s[:-2] + ");"          # strip trailing comma and space, and close the prototype
    else:                               
        return s + ");"

class FuncInfo(object):
    def __init__(self, classname, name, cname, rettype, isconstructor, ismethod, args):
        self.classname = classname
        self.name = name
        self.cname = cname
        self.isconstructor = isconstructor
        self.variants = []
        self.args = args
        self.rettype = rettype
        if ismethod:
            self_arg = ArgInfo((classname + "*","self", None, []))
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
        proto = "%s %s(" % (ret, full_fname)
        for arg in self.args:
            proto += "%s %s, " % (arg.tp, arg.name) 

        return close(proto)

    def gen_code(self):
        proto = self.get_wrapper_prototype()[:-1]
        code = "%s {\n" % (proto,)

        ret = "" if self.rettype == "void" else "return "
        prefix = ""
        postfix = self.cname
        if self.isconstructor:
            prefix = "new "
            postfix = self.classname
        elif self.ismethod:
            prefix = "self -> "
            
        call = prefix + "%s(" % (postfix,)

        for arg in self.args:
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
        self.code_funcs = StringIO()
        self.code_const_reg = StringIO()

    def add_func(self, decl):
        classname = bareclassname = ""
        name = decl[0]           # looks like cv{.classname}*.func
        dpos = name.rfind(".")
        if dpos >=0 and name[:dpos] != "cv":
            classname = bareclassname = re.sub(r"^cv\.", "", name[:dpos])
            name = name[dpos+1:]
            dpos = classname.rfind(".")
            if dpos >= 0:                               
                bareclassname = classname[dpos+1:]
                classname = classname.replace(".", "_")

        cname = name
        name = re.sub(r"^cv\.", "", name)
        isconstructor = cname == bareclassname
        ismethod = not isconstructor and bareclassname != ""

        cname = cname.replace(".", "::")

        args = map(ArgInfo, decl[3])
        
        if name in self.funcs.keys():
            #overloaded function...
            name += str(len(args))

        self.funcs[name] = FuncInfo(bareclassname, name, cname, 
                                    decl[1], isconstructor, ismethod, args)

    def save(self, path, name, buf):
        f = open(path + "/" + name, "wt")
        f.write(buf.getvalue())
        f.close()

    def gen(self, srcfiles, output_path):
        self.clear()
        parser = hdr_parser.CppHeaderParser()
        
        for hdr in srcfiles:
            decls = parser.parse(hdr)
            for decl in decls:
                name = decl[0]
                if name.startswith("struct") or name.startswith("class"):
                    pass
                elif name.startswith("const"):
                    pass
                else:
                    self.add_func(decl)

        funclist = list(self.funcs.items())
        funclist.sort()
        for name, func in funclist:
            code = func.gen_code()
            self.code_funcs.write(code)

        self.save(output_path, "opencv_generated_funcs.h", self.code_funcs)
        

if __name__ == "__main__":
    srcfiles = hdr_parser.opencv_hdr_list
    dstdir = "."
    if len(sys.argv) > 1:
        dstdir = sys.argv[1]
    if len(sys.argv) > 2:
        srcfiles = sys.argv[2:]

    generator = CWrapperGenerator()
    generator.gen(srcfiles, dstdir)

            
        
