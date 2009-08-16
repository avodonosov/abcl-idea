;; Source: http://www-106.ibm.com/developerworks/opensource/library/os-ecgui3/
;; Explorer v9

(cl:defpackage :swt0
  (:use :common-lisp 
	:java
	:jfli
	"java.lang"
	"java.net"
	"java.io"
	"org.eclipse.jface.action"
	"org.eclipse.jface.resource"
	"org.eclipse.jface.window"
	"org.eclipse.swt"
	"org.eclipse.swt.widgets"
	"org.eclipse.swt.custom"
	"org.eclipse.jface.viewers"
	"org.eclipse.swt.graphics")
  (:export :main))

(in-package :swt0)


(defparameter *image-registry* nil)

(defparameter *folder-image-url* nil)

(defparameter *file-image-url* nil)

(eval-when (:load-toplevel :execute)
  (let ((image-dir (pathname-directory *load-truename*)))
    (setq *folder-image-url* 
	  (file.tourl
	   (file.new (namestring (make-pathname :directory image-dir :name "folder" :type "gif"))))
	  *file-image-url* 
	  (file.tourl 
	   (file.new (namestring (make-pathname :directory image-dir :name "file" :type "gif")))))))

(defun get-image-registry ()
  (unless *image-registry*
    (setf *image-registry* (ImageRegistry.new))
    (ImageRegistry.put 
     *image-registry* "folder" 
     (ImageDescriptor.createFromURL
      *folder-image-url*))
    (ImageRegistry.put 
     *image-registry* "file" 
     (ImageDescriptor.createFromURL 
      *file-image-url*)))
  *image-registry*)
     

;; Implement some interfaces
(defparameter *tcp* ;;TreeContentProvider
  (flet ((get-children (element)
		       (let ((children (file.listfiles element)))
			 (if (null children)
			     (make-new-array "java.lang.Object" 0)
			   children))))
    (new-proxy 
     (ITreeContentProvider. ;the interface we're implementing
      (getChildren (element) (get-children element))
      (getElements (element) (get-children element))
      (hasChildren (element)
		   (plusp (jlength (get-children element))))
      (getParent (element)
		       (File.getParent element))))))



(defparameter *ftcp* ;;FileTableContentProvider
  (new-proxy
   (IStructuredContentProvider.
    (getElements (element) 
		 (let ((children (File.listFiles element)))
		   (if (null children)
		       (make-new-array "java.lang.Object" 0)
		       children))))))


(defparameter *ftlp* ;;FileTableLabelProvider
  (new-proxy
   (ITableLabelProvider.
    (getColumnText (element i)
		   (case i
		     (0 (File.getName element))
		     (1 (prin1-to-string (file.length element)))))
    (isLabelProperty (element string) nil)
    (getColumnImage (element column-index) 
		    (if (> column-index 0) 
			*null* ;;FIXME
			(if (File.isDirectory element)
			    (ImageRegistry.get (get-image-registry) "folder")
			    (ImageRegistry.get (get-image-registry) "file")))))))
			   
			     
;;We need to subclass org.eclipse.jface.viewers.LabelProvider...

(new-class
 "SWT0.FileTreeLabelProvider" ;;class-name
 "org.eclipse.jface.viewers.LabelProvider" ;;super (may be a list of super & interfaces)
 () ;;constructors
 ( ;; methods
  ("getText" "java.lang.String" nil (element)
             (File.getName element))
  ("getImage" image. nil ((element "java.lang.Object"))
	      (if (File.isDirectory element)
		  (ImageRegistry.get (get-image-registry) "folder")
		  (ImageRegistry.get (get-image-registry) "file"))))
 () ;;fields
 )


;; and a few other classes

(new-class
 "SWT0.FileSorter" 
 viewersorter.
 ()
 (("category" "int" nil (element)
	 (if (File.isDirectory element) 0 1)))
 ())

(new-class
 "SWT0.AllowOnlyFoldersFilter" 
 viewerfilter. 
 ()
 (("select" "boolean" nil (viewer parent element) (File.isDirectory element)))
 ())


(new-class 
 "SWT0.ExitAction" 
 action.
 (;;constructors
  (((w applicationwindow.))  
   (setf (ExitAction.window this) w)
   (ExitAction.setText this "E&xit@Ctrl+W")))
 (("run" :void nil () 
	 (ApplicationWindow.close (ExitAction.window this))))
 (("window" applicationwindow. :public)))


(new-class 
 "SWT0.Explorer" 
 applicationwindow. 
 (;;constructors
  (((shell shell.))
   (super shell) 
   (ApplicationWindow.addStatusLine this)
   (ApplicationWindow.addMenuBar this)))
	
 (;;methods
  ("createContents" 
   "org.eclipse.swt.widgets.Control"
   :protected
   ((parent "org.eclipse.swt.widgets.Composite"))
   (let* ((sash-form (sashform.new parent (boole boole-ior *swt.horizontal* *swt.null*)))
	  (tv (TreeViewer.new sash-form))
	  (tbv (TableViewer.new 
		sash-form 
		(boole boole-ior *swt.border*
		       (boole boole-ior *swt.full_selection* *swt.multi*)))))
     (Shell.setText
      (ApplicationWindow.getShell this)
      "ABCL File Explorer")
       
     (TableViewer.setContentProvider tbv *ftcp*)
     (TableViewer.setLabelProvider tbv *ftlp*)
     (TableViewer.setSorter tbv (FileSorter.new))
     (TreeViewer.setContentProvider tv *tcp*)
     (TreeViewer.setLabelProvider
      tv 
      (FileTreeLabelProvider.new))
     (TreeViewer.setInput tv (File.new (system.getproperty "user.home")))

     (TreeViewer.addFilter 
      tv 
      (AllowOnlyFoldersFilter.new))
	   
     (let ((column (TableColumn.new (TableViewer.getTable tbv) *swt.left*)))
       (TableColumn.setText column "Name")
       (TableColumn.setWidth column 200))

     (let ((column (TableColumn.new (TableViewer.getTable tbv) *swt.right*)))
       (TableColumn.setText column "Size")
       (TableColumn.setWidth column 100))
       
     (Table.setHeaderVisible
      (TableViewer.getTable tbv) t)
       
     (TreeViewer.addSelectionChangedListener
      tv
      (new-proxy
       (ISelectionChangedListener.
	(selectionChanged (event) 
			  (let* ((selection 
				  (SelectionChangedEvent.getSelection event))
				 (selected-file (IStructuredSelection.getFirstElement selection)))
			    (TableViewer.setInput tbv selected-file))))))
       
     (TableViewer.addSelectionChangedListener
      tbv
      (new-proxy
       (ISelectionChangedListener.
	(selectionChanged (event)
			  (let ((selection (SelectionChangedEvent.getSelection event)))
			    (ApplicationWindow.setStatus 
			     this
			     (format nil "Number of items selected is ~d" 
				     (istructuredselection.size selection))))))))
       
     sash-form))
  ("createMenuManager" 
   "org.eclipse.jface.action.MenuManager"
   :protected 
   ()
   (let ((bar-menu (MenuManager.new ""))
	 (file-menu (MenuManager.new "&File"))
	 (edit-menu (MenuManager.new "&Edit"))
	 (view-menu (MenuManager.new "&View")))
     (MenuManager.add bar-menu file-menu)
     (MenuManager.add bar-menu edit-menu)
     (MenuManager.add bar-menu view-menu)
     (MenuManager.add file-menu 
		      (ExitAction.new this))
     bar-menu)))
 ())



(defun main ()
  (setf *image-registry* nil)
  (let ((w (Explorer.new #+nil nil *null*)))
    (ApplicationWindow.setBlockOnOpen w t)
    (ApplicationWindow.open w)
    (Display.dispose (Display.getCurrent))))



