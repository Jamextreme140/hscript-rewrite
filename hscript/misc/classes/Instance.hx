package hscript.misc.classes;

import hscript.Interp.StaticInterp;
import hscript.Interp.ScriptRuntime;
import hscript.Ast.IHScriptCustomBehaviour;

/**
 * Provides handlers for custom classes.
 * 
 * @author Jamextreme140
 */
class Instance implements IHScriptCustomBehaviour {

    private var instanceInterp:Interp;
    private var classHandler:ClassHandler; // TODO: static fields lookup
    private var module:ScriptRuntime;

    public function new(args:Array<Dynamic>, classHandler:ClassHandler) {
        this.module = classHandler.module;
        this.classHandler = classHandler;
        build();

        if(instanceInterp.variables.exists('new')) {
            StaticInterp.callObjectField(null, instanceInterp.variables.get('new'), args);
        }
    }

    private function build() {
        instanceInterp = new InstanceInterp(classHandler.name);
        instanceInterp.errorHandler = module.errorHandler;
        instanceInterp.publicVariables = module.publicVariables;
        instanceInterp.execute(classHandler.clsDecl.body);
    }

    public function hget(field:String):Dynamic {
        return instanceInterp.variables.get(field);
    }

    public function hset(field:String, value:Dynamic):Dynamic {
        instanceInterp.variables.set(field, value);
        return value;
    }
}
