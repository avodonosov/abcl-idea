package abclidea;

import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.ConditionThrowable;

import java.net.URL;
import java.io.File;

public class Test {
    public static void main(String[] args) throws Throwable
    {


        String startLispFile = AbclIdea.getStartLispFile();
        String lispDir = new File(startLispFile).getParent() + File.separator;

        Interpreter interpreter = Interpreter.createInstance();
        //interpreter.eval("(format t \"Hello, world!~%\")");
        final String lispCommand = "(progn \n"
                                 + "  (defvar *lisp-dir* \"" + AbclIdea.escape(lispDir) + "\")\n"
                                 + "  (load \"" + AbclIdea.escape(startLispFile) + "\"))";

        interpreter.eval(lispCommand);
    }

/*
        public static void main(String[] args) throws Throwable
    {
        Interpreter interpreter = Interpreter.createInstance();
        //interpreter.eval("(compile nil '(lambda () (sleep 0.1)))");
        interpreter.eval("(compile-file \"C:/usr/unpacked/lisp-libs/slime/1.lisp\")");
    }
    */

}
