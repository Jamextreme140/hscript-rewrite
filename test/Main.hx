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
				public static var N:Int = 100;
				public static function getSum(a:Int, b:Int) {
					trace('static getSum');
					return a + b + MyClass.N;
				}

				public var a:Int = 50;
				public var b:Int = 17;
				public function new(a:Int, b:Int) {
					trace('hello! :3');
					trace('---- locals check ----');
					trace('this.a: ${this.a}');
					trace('a: $a');
					trace('this.b: ${this.b}');
					trace('b: $b');
					this.a = a;
					this.b = b;
				}

				public function sum() {
					var r = MyClass.getSum(a, b) * N;
					if(r == 67) return -1;
					return r;
				}
			}

			trace(MyClass.getSum(9, 10) + 2);
			var myInstance = new MyClass(6, 7);
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