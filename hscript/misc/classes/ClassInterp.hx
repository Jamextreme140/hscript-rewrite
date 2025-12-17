package hscript.misc.classes;

import hscript.Ast.VariableType;

class ClassInterp extends Interp {

    public function new(name:String) {
        super(name);
    }

    override function declarePublic(name:VariableType, value:Dynamic):Bool {
        return false;
    }

    override function declareStatic(name:VariableType, value:Dynamic):Bool {
        return false;
    }
}
