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

    private static inline var CLASS_POSFIX:String = "_HSX"; // BIG TODO

    public static function createInstance(cl:ClassHandler, args:Array<Dynamic>) {
        return cl.create(args);
    }

    public final name:String;
    public final isFinal:Bool = false; // TODO: final class
    public final classInterp:Interp;
    
    private var module:ScriptRuntime;
    private final clsDecl:ClassDecl;
	private final constructor:Dynamic;
    private var inheritance:Null<Dynamic> = null;

    public function new(clsDecl:ClassDecl, module:ScriptRuntime) {
        this.module = module;
        this.name = clsDecl.name;
        this.classInterp = new ClassInterp(this.name);
        this.clsDecl = clsDecl;
        this.constructor = Reflect.makeVarArgs(function(args) return this.create(args));
        if(clsDecl.extend != null)
            createInheritance();
        build();
    }

    private function createInheritance() {
        var extend:String = clsDecl.extend;
        if(module.variables.exists(extend)) {
            var cls:ClassHandler = module.variables.get(extend);
            if(cls.isFinal)
                throw 'Cannot extend a final class';
            inheritance = cls;
        }
        else
            inheritance = Type.resolveClass('${extend}$CLASS_POSFIX');

        if(inheritance == null)
            throw 'Invalid class: ${extend} was not found.';
    }

    private inline function build() {
        classInterp.errorHandler = module.errorHandler;
        classInterp.publicVariables = module.publicVariables;
        classInterp.execute(clsDecl.body);
    }

    private function create(args:Array<Dynamic>):Instance {
        return new Instance(args, this);
    }

    public function hasField(name:String) {
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
