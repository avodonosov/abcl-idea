;;;; The code below is example of how to say "Hello IDEA" from SLIME,
;;;; and similar fun things.
;;;;
;;;; It is expected to be executed after yanking.lisp is loaded, because
;;;; it uses some functions from yanking.lisp and assumes some java
;;;; classes are already DEF-JAVA-CLASS'ed.

(in-package :my)

;; Helper macro to run piece of code in IDEA event-dispatch thread.
;; (Lot of IDEA API methods may only be called from the event-dispatch thread).
(defmacro invoke-and-wait (&body body)
  `(let ((app (applicationmanager.getapplication))
         (runnable (make-runnable (lambda ()
                                    (restart-case
                                        (progn
                                          ,@body)
                                      ;; Lets create a restart, for the case if
                                      ;; we are running INVOKE-AND-WAIT from SLIME.
                                      ;; The BODY will be executed in IDEA
                                      ;; event-dispatch thread, which doesn't have
                                      ;; SLIME restarts. In case of errors in the
                                      ;; BODY we will trap into SLIME debugger, and
                                      ;; our restart will give us a way to
                                      ;; quit the SLIME debugger.
                                      (continue ()
                                        :report "Continue IDEA event-dispatch thread"
                                        nil))))))
     (application.invokeandwait app runnable (modalitystate.defaultmodalitystate))))


;; Just a messgae box
;;
;; (invoke-and-wait
;;   (messages.showmessagedialog "Hello from SLIME!"
;;                               "Hello, IDEA"
;;                               (messages.getInformationIcon)))

;; Expand or collapse all the folding regions in editor

(def-java-class "com.intellij.openapi.editor.FoldingModel")
(def-java-class "com.intellij.openapi.editor.FoldRegion")

(defun set-all-foldings-expanded (expanded-p)
  (let* ((fold-model (editor.getfoldingmodel (cur-editor-safe (cur-prj-safe))))
         (fold-regions  (foldingmodel.getallfoldregions fold-model))
         (runnable (runnable
                     (dotimes (i (jlength fold-regions))
                       (foldregion.setexpanded (jref fold-regions i)
                                               expanded-p)))))
    (foldingmodel.runbatchfoldingoperation fold-model
                                           runnable)))

;; Try it:
;;
;; (invoke-and-wait
;;   (set-all-foldings-expanded t))

;; Show or hide line number in the editor

(def-java-class "com.intellij.openapi.editor.EditorSettings")

(defun show-line-nums (show-p)
  (let ((ed-settings (editor.getsettings (cur-editor-safe (cur-prj-safe)))))
    (editorsettings.setlinenumbersshown ed-settings
                                        show-p)))
(defun toggle-line-nums ()
  (let* ((ed-settings (editor.getsettings (cur-editor-safe (cur-prj-safe))))
         (enabled-p (editorsettings.islinenumbersshown ed-settings)))
    (editorsettings.setlinenumbersshown ed-settings 
                                        (not enabled-p))))
;; Try it:
;;
;; (invoke-and-wait
;;   (show-line-nums nil))
;;
;; (invoke-and-wait 
;;   (toggle-line-nums))


