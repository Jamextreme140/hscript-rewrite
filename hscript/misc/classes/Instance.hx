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

    private var instanceInterp:InstanceInterp;
    private var classHandler:ClassHandler;
    private var module:ScriptRuntime;
    private var constructor:Dynamic = null;
    private var superClass:Dynamic = null;

    public function new(args:Array<Dynamic>, classHandler:ClassHandler) {
        this.module = classHandler.module;
        this.classHandler = classHandler;
        build();

        var hasSuperClass = classHandler.inheritance != null;
        if(instanceInterp.variables.exists('new')) {
            constructor = hasSuperClass ? createConstructor() : null;
            StaticInterp.callObjectField(null, instanceInterp.variables.get('new'), args);

            if(hasSuperClass && this.superClass == null)
                throw 'super() not called';
        }
        else if(hasSuperClass) {
            createSuperClass(args);
        }
    }

    private inline function build() {
        instanceInterp = new InstanceInterp(classHandler.name, classHandler);
        instanceInterp.errorHandler = module.errorHandler;
        instanceInterp.publicVariables = module.publicVariables;
        instanceInterp.scriptParent = this;
        instanceInterp.execute(classHandler.clsDecl.body);
    }

    private inline function createConstructor():Dynamic {
        return Reflect.makeVarArgs(createSuperClass);
    }

    private function createSuperClass(args:Array<Dynamic>) {
        if(classHandler.inheritance is ClassHandler) {
            var instance:Instance = ClassHandler.createInstance(classHandler.inheritance, args);
            superClass = instance;
            // TODO: fetch superclass fields to concatenate with "scriptParentFields"
        }
        else {
            superClass = Type.createInstance(classHandler.inheritance, args);
            // TODO: fetch superclass fields to concatenate with "scriptParentFields"
        }
    }

    public function hget(field:String):Dynamic {
        if(classHandler.hasField(field))
            throw 'The field $field should be accessed in a static way';
        return instanceInterp.resolveField(field);
    }

    public function hset(field:String, value:Dynamic):Dynamic {
        if(classHandler.hasField(field))
            throw 'The field $field should be accessed in a static way';
        return instanceInterp.assignField(field, value);
    }
}
