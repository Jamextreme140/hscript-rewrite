package hscript.misc.classes;

import hscript.Ast.IHScriptCustomBehaviour;

class Instance implements IHScriptCustomBehaviour {

    public function new(args:Array<Dynamic>, classHandler:ClassHandler) {}

    public function hget(field:String):Dynamic {
        throw "Not implemented";
    }

    public function hset(field:String, value:Dynamic):Dynamic {
        throw "Not implemented";
    }
}
