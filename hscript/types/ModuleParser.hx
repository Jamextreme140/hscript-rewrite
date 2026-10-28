package hscript.types;

import hscript.Ast.EImportMode;
import hscript.Ast.Expr;
import hscript.types.ModuleDecl;

// https://github.com/HaxeFoundation/hscript/blob/master/hscript/Parser.hx#L1187
class ModuleParser extends Parser {
    public function new(?module:String) {
        super(module);
    }

    public function parseModule(content:String):ModuleInfo {
        reset();
        this.tokens = Lexer.tokenize(content);
        
        final decls:Array<ModuleDecl> = [];
        while(true) {
            if (peekToken() == LTEof) break;
            decls.push(parseModuleDecl());
        }
        return {
            name: this.fileName,
            variableInfo: this.variablesList,
            decls: decls
        };
    }

    private function parseModuleDecl():ModuleDecl {
        final meta = parseMetadata();
        var ident = parseIdent();
        final isPrivate:Bool = ident == "private";
        if(isPrivate) ident = parseIdent();
        switch(ident) {
            case "package" | "using":
                final path = parsePath();
                ensure(LTSemiColon);
                return ident == "package" ? DPackage(path) : DUsing(path);
            case "import":
                final path = [parseIdent()];
                var wildCard:Bool = false;
                while(true) {
                    if(peekToken() != LTDot) break;
                    switch(readToken()) {
                        case LTIdentifier(id):
                            path.push(id);
                        case LTOp(MULT): // *
                            wildCard = true;
                        default: unexpected();
                    }
                }

                var name:String = null;
                if(!wildCard && (maybe(LTIdentifier("as")) || maybe(LTIdentifier("in")))) {
                    switch(readToken()) {
                        case LTIdentifier(id):
                            name = id;
                        default: unexpected();
                    }
                }

                final impMode:EImportMode = if (name != null) As(name) else if (wildCard) All else Normal;

                ensure(LTSemiColon);

                return DImport(path.join("."), impMode);
            case "class":
                final name = parseIdent();
                if(maybe(LTOp(LT)))
                    parseClassArgs();
                var extend:String = null;
                final implement:Array<String> = [];

                while(true) {
                    switch(readToken()) {
                        case LTIdentifier("extends"): 
                            extend = parseClassName();
                        case LTIdentifier("implements"):
                            implement.push(parseClassName());
                        default:
                            reverseToken();
                            break;
                    }
                }

                final fields:Array<FieldDecl> = [];
                ensure(LTOpenBr);
                while(!maybe(LTCloseBr))
                    fields.push(parseField());

                final staticFields:Array<FieldDecl> = [];
                for(f in fields.copy()) {
                    // TODO: optimize this with a O(1) implementation
                    if(f.access.contains(AStatic)) {
                        staticFields.push(f);
                        fields.remove(f);
                    }
                }
                return DClass({
                    name: variableID(name),
                    meta: meta,
                    extend: variableID(extend),
					implement: implement.map((i) -> return variableID(i)),
                    fields: fields,
                    staticFields: staticFields
                });
            case "typedef":
                final name = parseIdent();
                if(maybe(LTOp(LT)))
                    parseClassArgs();
                ensure(LTOp(ASSIGN));
                final t = parseType(); // parsed but not used
                return DTypedef(name, t);
            default:
                unexpected();
        }
        return null;
    }

    private function parsePath():String { 
        final identifiers:Array<String> = [];
        identifiers.push(parseIdent());

        while (true) {
            switch (readToken()) {
                case LTDot: identifiers.push(parseIdent());
                default: break;
            }
        }

        return identifiers.join(".");
    }

    private function parseMetaArgs() {
        if(peekToken() != LTOpenP) return null;

        final args:Array<Expr> = [];
        if(peekToken() != LTCloseP) {
            while(true) {
                args.push(parseExpr());
                switch(readToken()) {
                    case LTComma:
                    case LTCloseP: break;
                    default: unexpected();
                }
            }
        }

        return args;
    }

    private function parseMetadata():Metadata {
        final metas:Array<MetadataEntry> = [];

        while(true) {
            switch(readToken()) {
                case LTMeta(meta):
                    metas.push({
                        name: meta,
                        params: parseMetaArgs()
                    });
                default:
                    reverseToken();
                    break;
            }
        }

        return { metas: metas };
    }

    private function parseField():FieldDecl {
        final meta = parseMetadata();
        final access:Array<FieldAccess> = [];
        while(true) {
            final id = parseIdent();
            switch(id) {
                case APublic | APrivate | AInline | AOverride | AStatic | AMacro:
                    access.push(id);
                case "function":
                    final name = parseIdent();
                    final fnDecl = parseFunctionDecl();
                    return {
                        name: variableID(name),
                        meta: meta,
                        access: access,
                        kind: KFunction(fnDecl)
                    };
                case "var" | "final":
                    final name = parseIdent();
                    var get:String = null;
                    var set:String = null;
                    if(maybe(LTOpenP)) {
                        get = parseIdent();
                        ensure(LTComma);
                        set = parseIdent();
                        ensure(LTCloseP);
                    }
                    if(maybe(LTColon)) parseType();
                    final expr = maybe(LTOp(ASSIGN)) ? parseExpr() : null;

                    if(expr != null) {
                        if(isBlock(expr)) maybe(LTSemiColon);
                        else ensure(LTSemiColon);
                    }
                    return {
                        name: variableID(name),
                        meta: meta,
                        access: access,
                        kind: KVar({
                            expr: expr,
                            getter: get,
                            setter: set
                        })
                    };
                default:
                    unexpected();
                    break;
            }
        }
        return null;
    }

    private function parseFunctionDecl():FuncDecl {
        ensure(LTOpenP);
        final args = parseFunctionArgs();
        if(maybe(LTColon)) parseType();
        return {
            args: args,
            expr: parseExpr()
        }
    }
}