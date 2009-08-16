package abclidea;

import com.intellij.openapi.components.ApplicationComponent;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.annotations.NotNull;

import java.net.URL;
import java.net.JarURLConnection;
import java.io.IOException;

public class AbclIdea implements ApplicationComponent {
    public AbclIdea() {
    }

    public void initComponent() {
        // TODO: insert component initialization logic here
    }

    public void disposeComponent() {
        // TODO: insert component disposal logic here
    }

    @NotNull
    public String getComponentName() {
        return "AbclIdea";
    }

    public static String getStartLispFile()
    {
        // find file lisp/start.lisp
        // based on path to our plugin classes

        final String classFile = "/abclidea/AbclIdea.class";
        URL url = Test.class.getResource(classFile);
        if (url == null)
        {
            throw new RuntimeException("Can't find directory \"lisp\" because file "
                                       + classFile + " is absent in classpath");
        }

        String pathTailToReplace;

        if ("jar".equals(url.getProtocol()))
        {
            // If our plugin is packaged into .jar, we expect Lisp code
            // to be in the "lisp" directory beside the .jar.

            pathTailToReplace = "lib/abcl-idea.jar";

            try {
                JarURLConnection jarUrlConnection = ((JarURLConnection)url.openConnection());
                url = jarUrlConnection.getJarFileURL();
            } catch (IOException e) {
                throw new RuntimeException("Can't find url or the ABCL plugin jar file", e);
            }
        }
        else
        {
            // Otherwise, if plugin classes are not packaged into .jar
            // we expect the "lisp" directory to be beside the AbclIdea.class file.
            // This usually is the case during the plugin development, when you
            // start Run/Debug plugin configuration from IDEA.

            pathTailToReplace = "AbclIdea.class";
        }

        if (!"file".equals(url.getProtocol()))
        {
            throw new RuntimeException("Can't find directory \"lisp\". We want to find it by relative " +
                                       "path to the " + pathTailToReplace + " file, but we can't handle the " +
                                       "URL returned by classloader." +
                                       "Expected URL schema is \"file\", but actual URL is " + url);
        }

        String path = url.getPath();
        return path.substring(1, path.lastIndexOf(pathTailToReplace)) + "lisp/start.lisp";
    }

    // copy/pasted from AbclScriptEngine which is not
    // compiled by default with abcl (but only when
    // you have JSR-223 jars in your classpath during build)
    public static String escape(String s) {
        StringBuffer b = new StringBuffer();
        int len = s.length();
        char c;
        for (int i = 0; i < len; ++i) {
            c = s.charAt(i);
            if (c == '\\' || c == '"') {
                b.append('\\');
            }
            b.append(c);
        }
        return b.toString();
    }
}
