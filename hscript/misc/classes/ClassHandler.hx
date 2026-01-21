package hscript.misc.classes;

import hscript.Ast;
import hscript.Ast.ClassDecl;
import hscript.Interp;
import hscript.Ast.IHScriptCustomBehaviour;

/**
 * Provides handlers for static class fields and instantiation.
 */
 @:allow(hscript.misc.classes.Instance)
class ClassHandler implements IHScriptCustomBehaviour {

    public static function createInstance(cl:ClassHandler, args:Array<Dynamic>) {
        return cl.create(args);
    }

    public final name:String;

    private var module:ScriptRuntime;
    private var classInterp:Interp;
    private final clsDecl:ClassDecl;
	private final constructor:Dynamic;

    public function new(clsDecl:ClassDecl, module:ScriptRuntime) {
        this.module = module;
        this.name = clsDecl.name;
        this.classInterp = new ClassInterp(this.name);
        this.clsDecl = clsDecl;
        this.constructor = Reflect.makeVarArgs(function(args) return this.create(args));
        build();
    }

    private function build() {
        classInterp.errorHandler = module.errorHandler;
        classInterp.publicVariables = module.publicVariables;
        classInterp.execute(clsDecl.body);
    }

    private function create(args:Array<Dynamic>):Dynamic {
        return new Instance(args, this);
    }

    private inline function hasField(name:String) {
        return classInterp.variables.exists(name);
    }

    public function hget(field:String):Dynamic {
        if(field == 'new') 
            return constructor;
        
        return classInterp.variables.get(field);
    }

    public function hset(field:String, value:Dynamic):Dynamic {
        classInterp.variables.set(field, value);
        return value;
    }

    public function toString() {
        return name;
    }
}
