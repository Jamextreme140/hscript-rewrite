package hscript.types;

import hscript.types.ModuleDecl.ModuleInfo;

class Module {
    public var info:ModuleInfo;
    public var interp:Interp;

    public function new(info:ModuleInfo) {
        this.info = info;
        this.interp = new Interp(info.name);
        this.interp.loadTables(info.variableInfo);
        this.interp.loadBaseVariables();
    }
}