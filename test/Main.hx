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
			class MyClass {
				public static function getSum(a:Int, b:Int) {
					return a + b + 2;
				}

				public var a:Int;
				public var b:Int;
				public function new(n1:Int, n2:Int) {
					trace('hello! :3');
					a = n1;
					b = n2;
				}

				public function sum() {
					var r = a + b;
					if(r == 67) return -1;
					return r;
				}
			}

			trace(MyClass.getSum(9, 10));
			var myInstance = new MyClass(50, 17);
			var v = myInstance.sum();
			trace(v == -1 ? 'nope :>' : v);
		");

		expr = Analyzer.optimize(expr);
		trace("------ HSCRIPT AST ------");
		trace(hscript.utils.ExprUtils.print(expr));
		trace("--------- OUTPUT --------");
		var interp:Interp = new Interp("Main.hx");
		interp.errorHandler = (error:Error) -> {Sys.println(error);}
		interp.execute(expr);
    }
}