package abclidea;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.openapi.wm.ToolWindowAnchor;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessOutputTypes;
import com.intellij.peer.PeerFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.armedbear.lisp.*;

import javax.swing.*;
import java.io.*;

public class StartAction extends AnAction {

    public void actionPerformed(AnActionEvent e) {

        Thread abclThread = new Thread("ABCL/IDEA Swank thread") {
            public void run() {

                Interpreter interpreter = Interpreter.createInstance();

                final AbclProcessHandler abclProcessHandler = new AbclProcessHandler(interpreter);
                createConsoleWindow(abclProcessHandler);

                final String startLispFile = AbclIdea.getStartLispFile();
                final String lispDir = new File(startLispFile).getParent() + File.separator;

                try
                {
                    interpreter.eval("(defpackage :abclidea (:use :cl) (:export *lisp-dir*))");

                    final String progn =
                            "(progn \n"
                            + "  (defvar abclidea:*lisp-dir* \"" + AbclIdea.escape(lispDir) + "\")\n"
                            + "  (load \"" + AbclIdea.escape(startLispFile) + "\"))";

                    interpreter.eval(progn);
                }
                catch(ConditionThrowable conditionThrowable)
                {
                    msg(conditionThrowable.getMessage(), "Error", Messages.getInformationIcon());
                }
            }
        };
        abclThread.start();
    }

    private Project curPrj() {
        Project allProjects[] = ProjectManager.getInstance().getOpenProjects();
        if (allProjects.length > 0)
            return allProjects[0];
        return null;
    }

    private void msg(final String msg, final String title, final Icon icon) {

        ApplicationManager.getApplication().invokeAndWait(
                new Runnable() {
                    public void run() {
                        Messages.showMessageDialog(msg, title, icon);
                    }
                },
                ModalityState.defaultModalityState());


    }
    private void createConsoleWindow(final ProcessHandler abclProcessHandler) {
        final Project curPrj = curPrj();
        if (curPrj == null) {
            // TODO: if we can't create IDEA console in absense of project, we may at least show some simple window that displays ABCL's output
            msg("You will not see ABCL output because we can not create "
                        + "\"ABCL Console\" tool window as you don't have a project opened. "
                        + "(You may fix it: go to the StartAction.java in the plugin sources, "
                        + "see TODO comment and implement a kind of window for showing ABCL "
                        + "output in case of absence of opened project)",
                "Warning",
                Messages.getInformationIcon());
        } else {
            Runnable consolreCreator = new Runnable() {
                public void run() {
                    ToolWindowManager toolMan = ToolWindowManager.getInstance(curPrj);
                    ToolWindow toolWnd = toolMan.registerToolWindow("ABCL Console",
                                                                    true,
                                                                    ToolWindowAnchor.BOTTOM);
                    toolWnd.setTitle("ABCL Console");

                    TextConsoleBuilderFactory builderFac = TextConsoleBuilderFactory.getInstance();
                    ConsoleView console = builderFac.createBuilder(curPrj).getConsole();
                    console.attachToProcess(abclProcessHandler);

                    PeerFactory peerFac = com.intellij.peer.PeerFactory.getInstance();
                    ContentFactory contentFac = peerFac.getContentFactory();
                    Content content = contentFac.createContent(console.getComponent(), "", false);
                    toolWnd.getContentManager().addContent(content);
                    toolWnd.activate(null);
                }
            };

            ApplicationManager.getApplication().invokeAndWait(consolreCreator,
                                                              ModalityState.defaultModalityState());

        }
    }
}

class AbclProcessHandler extends ProcessHandler {

    private PipedOutputStream abclInputOurEnd;
    private Interpreter interpreter;

    public AbclProcessHandler(Interpreter interpreter)
    {
        this.interpreter = interpreter;

        this.abclInputOurEnd = new PipedOutputStream();
        PipedInputStream abclInputAbclsEnd;
        try {
            abclInputAbclsEnd = new PipedInputStream(this.abclInputOurEnd);
        } catch (IOException e) {
            e.printStackTrace();
            return;
        }

        Lisp.resetIO(new Stream(abclInputAbclsEnd, Symbol.CHARACTER),
                     new Stream(this.abclOutputReceiver, Symbol.CHARACTER));
    }

    protected void destroyProcessImpl() {
        // I don't really know what is this method for.
        // Seems to be for stoppig the process (ABCL),
        // but I don't know when it is called.
        // Console tool window doesn't have any
        // controls for closing.

        // the code below shoult stop ABCL (taken from Interpreter.kill method),
        // but it hasn't been tested.
        // TODO: refactor Interprettor class for good ABCX in/out stream closing without System.exit (which is currenly only implemented for case when ABCL is embeded into J editor) 
        try {
            abclInputOurEnd.close();
        } catch (IOException e) {
            Logger.ourFactory.getLoggerInstance(this.getClass().getName()).error(e);
        }
        try {
            abclOutputReceiver.close();
        } catch (IOException e) {
            Logger.ourFactory.getLoggerInstance(this.getClass().getName()).error(e);
        }
        interpreter.dispose();
    }

    protected void detachProcessImpl() {
        // what must be here?
    }

    public boolean detachIsDefault() {
        // what must we return?
        return false;
    }

    public OutputStream getProcessInput() {
        return abclInputOurEnd;
    }

    private OutputStream abclOutputReceiver = new OutputStream()
    {
        public void write(int b) throws IOException {
            notifyTextAvailable("" + (char)b, ProcessOutputTypes.STDOUT);
        }

        public void write(byte b[], int off, int len) throws IOException {
            notifyTextAvailable(new String(b, off, len), ProcessOutputTypes.STDOUT);
        }
    };
};
