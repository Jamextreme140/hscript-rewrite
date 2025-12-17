package hscript.misc.classes;

import hscript.Ast;
import hscript.Ast.ClassDecl;
import hscript.Interp;
import hscript.Ast.IHScriptCustomBehaviour;

/**
 * Provides handlers for static class fields and instantiation.
 */
class ClassHandler implements IHScriptCustomBehaviour {

    public static function createInstance(cl:ClassHandler, args:Array<Dynamic>) {
        return cl.create(args);
    }

    public final name:String;

    private var module:ScriptRuntime;
    private var classInterp:Interp;

    public function new(clsDecl:ClassDecl, module:ScriptRuntime) {
        this.module = module;
        this.name = clsDecl.name;
        build(clsDecl);
    }

    private function build(clsDecl:ClassDecl) {
        
    }

    private function create(args:Array<Dynamic>):Dynamic {
        return null;
    }

    public function hget(field:String):Dynamic {
        throw "Not implemented";
    }

    public function hset(field:String, value:Dynamic):Dynamic {
        throw "Not implemented";
    }

    public function toString() {
        return name;
    }
}
