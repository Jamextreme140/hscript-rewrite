package hscript.types;

import hscript.Ast.VariableInfo;
import hscript.Ast.VariableType;
import hscript.Ast.Expr;
import hscript.Ast.Argument;
import hscript.Ast.EImportMode;

// most of this from official hscript
// https://github.com/HaxeFoundation/hscript/blob/master/hscript/Expr.hx#L123

@:structInit
class ModuleInfo {
    public var name:String;
    public var variableInfo:VariableInfo;
    public var decls:Array<ModuleDecl>;
}

enum ModuleDecl {
    DPackage(path:String);
    DImport(path:String, mode:EImportMode);
    DUsing(path:String);
    DClass(c:ClassDecl);
    DTypedef(name:String, ?type:String);
}

@:structInit
class MetadataEntry {
    public var name:String;
    public var params:Array<Expr>;
}

@:structInit
class Metadata {
    public var metas:Array<MetadataEntry>;
}

@:structInit
class ModuleType {
    public var name:VariableType;
    public var meta:Metadata;
}

@:structInit
class ClassDecl extends ModuleType {
    public var extend:VariableType;
    public var implement:Array<VariableType>;
    public var fields:Array<FieldDecl>;
    public var staticFields:Array<FieldDecl>;
}

@:structInit
class FieldDecl {
    public var name:VariableType;
    public var meta:Metadata;
    public var kind:FieldKind;
    public var access:Array<FieldAccess>;
}

enum abstract FieldAccess(String) from String to String {
    var APublic = "public";
    var APrivate = "private";
    var AInline = "inline";
    var AOverride = "override";
    var AStatic = "static";
    var AMacro = "macro";
}

enum FieldKind {
    KFunction(f:FuncDecl);
    KVar(v:VarDecl);
}

@:structInit
class FuncDecl {
    public var args:Array<Argument>;
    public var expr:Expr;
}

@:structInit
class VarDecl {
    public var expr:Null<Expr>;
    public var getter:Null<String>;
    public var setter:Null<String>;
}