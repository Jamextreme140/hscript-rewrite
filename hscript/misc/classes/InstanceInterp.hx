package hscript.misc.classes;

class InstanceInterp extends Interp {

    public function new(name:String) {
        super(name);
    }

    override function interpExpr(expr:Ast.Expr):Dynamic {
        if (expr == null) return null;

        this.lineNumber = expr.line;

        return switch(expr.expr) {
            case EVar(name, init, _, isStatic):
                if(depth == 0 && isStatic) // Class variable, ignore it.
                    return null;
                declare(name, init == null ? null : interpExpr(init));
                return null;
            case EFunction(args, body, name, _, isStatic):
                if(name != -1) {
                    if(depth == 0 && isStatic) // Class function, ignore it.
                        return null;
                }
                interpFunction(args, body, name, false, false);
            default: super.interpExpr(expr);
        }
    }

    // TODO: Lookup for static fields 
    override function resolveGlobal(ident:Ast.VariableType):Dynamic {
        return super.resolveGlobal(ident);
    }
}
