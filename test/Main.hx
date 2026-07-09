package;

import hscript.Ast.Expr;
import hscript.anaylzers.Analyzer;
import hscript.Error;
import hscript.Interp;
import hscript.Parser;

using hscript.utils.ExprUtils;

class Module {
	// makes persisten class fields values :3
	private static var moduleCache:Map<String, Module> = [];
	// fixed script lookup. It should fetch real script files to use
	public static function importFailedCallback(path:String):Dynamic {
		var module:Module = null;
		var splitPath = path.split(".");

		if(moduleCache.exists(path)) 
			return moduleCache.get(path).interp.variables.get(splitPath[splitPath.length - 1]);
		
		switch(path) {
			case "pack.MyOtherClass":
				var code:String = "
					import haxe.ds.StringMap;

					class HelperClass {
						public static function sum(a:Int, b:Int):Int {
							return a + b;
						}
					}

					class MyOtherClass {
						public static var NUM:Int = 10;
						var map:StringMap<Int>;
						public function new(a:Int, b:Int) {
							map = new StringMap<Int>();
							trace('Hello again :3');
							trace(map.get(':3') == null);
							trace(HelperClass.sum(a, b));
						}

						public function iGotThis() {
							return '    FAHHHH!!   ';
						}
					}
				";
				module = new Module(code, path);
				module.load();
				moduleCache.set(path, module);
			default:
		}
		return module != null ? module.interp.variables.get(splitPath[splitPath.length - 1]) : null;
	}
	
	private var interp:Interp;
	private var parser:Parser;
	private var expr:Expr;
	private var loaded:Bool = false;

	public function new(code:String, path:String) {
		parser = new Parser();
		interp = new Interp(path);
		interp.errorHandler = (error:Error) -> {Sys.println(error);}

		parse(code);
	}

	private function parse(code:String) {
		try {
			if(code != null && StringTools.trim(code) != "") {
				expr = parser.parseString(code);
				expr = Analyzer.optimize(expr);
			}
		}
		catch(e:hscript.Error) {
			trace(e.toString());
		}
		catch(e) {
			trace(e.toString());
		}
	}

	public function load() {
		if(loaded) return;

		if(expr != null)
			interp.execute(expr);

		loaded = true;
	}
}

class Main {
    public static function main() {
        StaticInterp.pathResolver = Module.importFailedCallback;

        var code = "
			import pack.MyOtherClass;
			import StringTools;

			class MyClass {
				private var moc:MyOtherClass;

				public function new() {
					trace(MyOtherClass.NUM);
					MyOtherClass.NUM += 20;
					moc = new MyOtherClass(50, 50);
					var fah = moc.iGotThis();
					trace(fah);
					trace(StringTools.trim(fah));
					trace(MyOtherClass.NUM);
				}
			}

			private var myClass = new MyClass();
        ";
        var module1 = new Module(code, "Main.hx");
        module1.load();	
    }
}