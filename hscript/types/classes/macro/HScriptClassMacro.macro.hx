package hscript.types.classes.macro;

import haxe.macro.Type.ClassType;
import Type.ValueType;
import haxe.macro.Expr.Function;
import haxe.macro.Expr;
import haxe.macro.Type.MetaAccess;
import haxe.macro.Type.FieldKind;
import haxe.macro.Type.ClassField;
import haxe.macro.Type.VarAccess;
import haxe.macro.*;

import Type as HaxeType;

using StringTools;

class HScriptClassMacro {
    public static inline final FINISHED_META:String = ":customClassBuilt"; 

    public static function init() {
        #if !display
		#if CUSTOM_CLASSES
		if(Context.defined("display")) return;
		/* for(apply in Config.ALLOWED_CUSTOM_CLASSES) {
			Compiler.addGlobalMetadata(apply, "@:build(hscript.macros.ClassExtendMacro.build())");
		} */
		//Context.onAfterTyping(buildTyped);
		#end
		#end
    }

    public static final unallowedMetas:Array<String> = [
        ":bitmap", 
        ":noCustomClass", 
        ":generic", 
        ":coreApi", // Core api classes require type which can't be specified on runtime.
        ":structInit", // Classes specified to act as anonymous structures shouldn't be extended.
        ":nativeGen" // :nativeGen makes the class get treated as an extern, so it shouldn't be extended.
    ];

    public static function build():Array<Field> {
        var clRef = Context.getLocalClass();
		if (clRef == null) return null; // Do nothing; skip.
		var cl:ClassType = clRef.get();
        var pos:Position = Context.currentPos();

        // Make sure the macro doesn't run twice on the class.
        if (cl.meta.has(FINISHED_META)) return null;
        cl.meta.add(FINISHED_META, [], pos);

        // Omit being able to extend some classes.
        if (cl.isInterface || cl.isAbstract || cl.isExtern || cl.isFinal) return null;

        if(!HaxeType.enumEq(cl.kind, KNormal)) return null;

        for(m in cl.meta.get()) 
            if(unallowedMetas.contains(m.name))
                return null;
        
        final clsName:String = formatClassString(cl);
        //trace('class to modify: $clsName');

        final neededInstFields:Array<String> = [
            '__instance',
            '__skipCheckFrom',
        ];

        var fields:Array<Field> = Context.getBuildFields().copy();

        for(f in fields) {
            if(cl.superClass == null && neededInstFields.contains(f.name)) {
                Context.info(
                    'HScriptClassMacro: Couldn\'t build hscript fields for the class $clsName since it already has the instance field ${f.name}.',
                    pos
                );
                return null;
            }
        }

        return fields;
    }

    static function formatClassString(cls:ClassType):String {
        return cls.pack.copy().concat([cls.name]).join('.');
    }
}