package hscript.misc.classes;

import hscript.Ast.VariableType;

class ClassInterp extends Interp {

    private var handler:ClassHandler;

    public function new(name:String, handler:ClassHandler) {
        this.handler = handler;
        super(name);
    }

    override function interpExpr(expr:Ast.Expr):Dynamic {
        if (expr == null) return null;

        this.lineNumber = expr.line;

        return switch(expr.expr) {
            case EVar(name, init, _, isStatic):
                if(depth == 0 && !isStatic) // Instance variable, ignore it.
                    return null;
                declare(name, init == null ? null : interpExpr(init));
                return null;
            case EFunction(args, body, name, _, isStatic):
                if(name != -1) {
                    if(depth == 0 && !isStatic) // Instance function, ignore it.
                        return null;
                }
                interpFunction(args, body, name, false, false);
            default: super.interpExpr(expr);
        }
    }

    override function resolveGlobal(ident:VariableType):Dynamic {
        var varName:String = variableNames[ident];
        if(handler.hasInModule(varName))
            return handler.getFromModule(varName);
        if(handler.hasImport(varName)) {
            var i:Ast.ImportInfo = handler.resolveImport(varName);
            return interpImport(i.path, i.mode);
        }
        return super.resolveGlobal(ident);
    }
}
