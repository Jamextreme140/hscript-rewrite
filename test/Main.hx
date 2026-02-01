package;

import haxe.Timer;
import hscript.bytecode.ByteInstruction.ByteChunk;
import hscript.anaylzers.Analyzer;
import hscript.bytecode.ByteCompiler;
import hscript.bytecode.ByteVM;
import hscript.utils.BytesPrinter;
import hscript.Error;
import hscript.Interp;
import hscript.Parser;

using hscript.utils.ExprUtils;

class Main {
    public static function main() {
        var parser = new Parser();
        var expr = parser.parseString("
			import haxe.ds.StringMap;

			//var outsideMap = new StringMap<Int>();

			class MyMap {
				var map:StringMap<Int>;
				public function new() {
					map = new StringMap();
				}

				public function get(s:String) {
					return map.get(s);
				}

				public function set(s:String, v:Int) {
					map.set(s, v);
					return v;
				}
			}

			var mm = new MyMap();
			trace(mm.set(':3', 9));
			trace(mm.get(':3'));
		");

		//expr = Analyzer.optimize(expr);
		trace("------ HSCRIPT AST ------");
		trace(hscript.utils.ExprUtils.print(expr));
		trace("--------- OUTPUT --------");
		var interp:Interp = new Interp("Main.hx");
		interp.errorHandler = (error:Error) -> {Sys.println(error);}
		interp.execute(expr);
    }
}