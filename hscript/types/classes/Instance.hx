package hscript.types.classes;

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

        final hasSuperClass:Bool = classHandler.hasClassReference;
        if(instanceInterp.variables.exists('new')) {
            constructor = hasSuperClass ? createConstructor() : null;
            StaticInterp.callObjectField(null, instanceInterp.variables.get('new'), args);

            if(hasSuperClass && this.superClass == null)
                throw 'missing super constructor call';
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
        if(classHandler.classReference is ClassHandler) {
            var superInstance:Instance = ClassHandler.createInstance(classHandler.classReference, args);
            superClass = superInstance;
            for(f => name in superInstance.instanceInterp.variablesLookup) {
                @:privateAccess
                if(superInstance.instanceInterp.instanceVariablesDeclared[name]) 
                    this.instanceInterp.scriptParentFields.set(f, true);
            }
        }
        else {
            // create macro-generated super class instance
            superClass = Type.createInstance(classHandler.classReference, args);
            for(f in Type.getInstanceFields(classHandler.classReference))
                this.instanceInterp.scriptParentFields.set(f, true);
            superClass.instance = this;
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
