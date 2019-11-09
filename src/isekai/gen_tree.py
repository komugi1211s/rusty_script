import sys
from os import path

GEN_FILENAME = "generate.rs"
trait = """ 
trait Expr {
    fn interpret();
}

"""

def defineAst(_path, base):
    filepath = path.join(_path, GEN_FILENAME)
    
    with open(filepath, "w") as fi:
        fi.write(trait)
        
        for name, fields in TYPES.items():
            fi.write("struct %s {\n" % name)
            for field in fields:
                fi.write("    {},\n".format(field))

            fi.write("}\n\n")

            if name.endswith("<EX>"):
                fi.write("impl<EX: Expr> %s {\n" % name)

            else:
                fi.write("impl %s {\n" % name)

            fi.write("    pub fn new(" + ", ".join(fields) + ") -> Self {\n")
            fi.write("        Self {\n")
            for field in fields:
                fi.write("            {},\n".format(field.split(":")[0]))

            fi.write("        }\n")
            fi.write("    }\n")
            fi.write("}\n\n")
            

TYPES = {
    "Binary<EX>": ["left: Option<EX>", "right: Option<EX>", "oper: TokenType"],
    "Grouping<EX>": ["expr: Option<EX>"],
    "Literal": ["value: TokenType"],
    "Unary<EX>": ["oper: TokenType", "right: Option<EX>"],
}

def main():
    if len(sys.argv) <= 1:
        print("Usage: gen_tree.py dirname")
        return

    defineAst(sys.argv[1], "Expr")

main()
