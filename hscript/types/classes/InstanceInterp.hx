package hscript.types.classes;

import hscript.Ast;
import hscript.Interp;
import haxe.ds.Vector;

class InstanceInterp extends Interp {

    /**
     * Shares the same array length as `variablesDeclared` and `variablesValues`
     * but current variables (at any scope) are isolated from the instance variables
     * allowing to do things like `this.a = a;`
     */
    private var instanceVariablesDeclared:Vector<Bool>;
    private var instanceVariablesValues:Vector<IVariableReference>;

    private var classHandler:ClassHandler;

    public function new(name:String, classHandler:ClassHandler) {
        this.classHandler = classHandler;
        super(name);
    }

    private override function loadTables(info:VariableInfo) {
        instanceVariablesDeclared = new Vector<Bool>(info.length);
        instanceVariablesValues = new Vector<IVariableReference>(info.length);
        super.loadTables(info);
    }

    public override function loadBaseVariables() {
        // Allow access to class (static) variables 
        variables.set(classHandler.name, classHandler); 
        super.loadBaseVariables();
    }

    private override function interpExpr(expr:Ast.Expr):Dynamic {
        if (expr == null) return null;

        this.lineNumber = expr.line;

        return switch(expr.expr) {
            case EIdent(name):
                if (variablesDeclared[name]) variablesValues[name].r; 
                else if(instanceVariablesDeclared[name]) instanceVariablesValues[name].r;
                else resolveGlobal(name);
            case EVar(name, init, _, isStatic):
                if(depth == 0 && isStatic) // Class variable, ignore it.
                    return null;
                __declare(name, init == null ? null : interpExpr(init));
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
    // same function from `declare` since is inlined 
    private inline function __declare(name:VariableType, value:Dynamic):Dynamic {
        var v:IVariableReference = {r: value};
        if (depth != 0) {
            changes.push({
                name: name,
                oldDeclared: variablesDeclared[name],
                oldValue: variablesValues[name]
            });

            variablesDeclared[name] = true;
            variablesValues[name] = v;
        } 
        else {
            // instance field declaration
            instanceVariablesDeclared[name] = true;
            instanceVariablesValues[name] = v;
        }

        return value;
    }

    private inline function __declareFunction(name:VariableType, ref:IVariableReference) {
        if (depth != 0){
            changes.push({
                name: name,
                oldDeclared: variablesDeclared[name],
                oldValue: variablesValues[name]
            });

            variablesDeclared[name] = true;
            variablesValues[name] = ref;
        } 
        else {
            // instance field declaration
            instanceVariablesDeclared[name] = true;
            instanceVariablesValues[name] = ref;
        }

        return ref.r;
    }

    private override function interpFunction(args:Array<Argument>, body:Expr, name:VariableType, ?isPublic:Bool, ?isStatic:Bool) {
        var fn:Dynamic = super.interpFunction(args, body, name, isPublic, isStatic);
        var fnRef:IVariableReference = {r: null};
        if(name != -1 && depth == 0) {
            // instance function declaration
            fnRef.r = fn;
            __declareFunction(name, fnRef);
        }
        return fn;
    }

    public function resolveField(ident:String):Dynamic {
        if(!variablesLookup.exists(ident)) return null;
        var name = variablesLookup.get(ident);
        return instanceVariablesDeclared[name] ? instanceVariablesValues[name].r : null;
    }

    public function assignField(ident:String, value:Dynamic):Dynamic {
        if(!variablesLookup.exists(ident)) return null;
        var name = variablesLookup.get(ident);
        instanceVariablesDeclared[name] = true;
        instanceVariablesValues[name].r = value;

        return assign(name, value);
    }

    override function resolveGlobal(ident:VariableType):Dynamic {
        var varName:String = variableNames[ident];
        if(varName == 'this')
            return scriptParent;
        if(classHandler.hasField(varName))
            return classHandler.classInterp.variables.get(varName);
        if(classHandler.hasInModule(varName))
            return classHandler.getFromModule(varName);
        return super.resolveGlobal(ident);
    }
}
