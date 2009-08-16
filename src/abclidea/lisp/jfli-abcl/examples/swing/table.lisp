(cl:defpackage :table
  (:use :common-lisp
	:jfli
	"java.lang"
	"java.sql"
	"java.util"
	"javax.swing.table"
	"javax.swing"
	"java.awt")
  (:export :create-and-show-gui))

(in-package :table)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (class.forname "org.postgresql.Driver") ;loads the driver
  )




(new-class 
 "TABLE.MyTableModel0"
 abstracttablemodel. 
 ((((query "java.lang.String"))
   (let* ((conn (drivermanager.getconnection "jdbc:postgresql:mydb" "simon" ""))
	  (pstmt
	   (connection.preparestatement conn query
					*resultset.type_scroll_insensitive* *resultset.concur_updatable*))
	  (result-set (preparedstatement.executequery pstmt))
	  (meta-data (preparedstatement.getmetadata pstmt)))

     (setf (MyTableModel0.conn this) conn)
     (setf (MyTableModel0.pstmt this) pstmt)
     (setf (MyTableModel0.rs this) result-set)
     (setf (MyTableModel0.rsMetaData this) meta-data)
     (setf (MyTableModel0.noOfColumns this) (resultsetmetadata.getcolumncount meta-data)))))
	
 (("getColumnCount" :int  :public () 
		    (MyTableModel0.noOfColumns this))
  ("getRowCount" :int :public () 
		 (let ((result-set (MyTableModel0.rs this)))
		   (resultset.last result-set) ;move to last
		   (resultset.getrow result-set)))

  ("getColumnName" "java.lang.String" :public ((i :int))
		   (resultsetmetadata.getcolumnname (MyTableModel0.rsMetaData this) (1+ i)))
	   
  ("getValueAt" object. :public ((row :int) (col :int))
		(let ((result-set (MyTableModel0.rs this)))
		  (resultset.absolute result-set (1+ row))
		  (or (resultset.getobject result-set (1+ col)) *null*)))

  ("getColumnClass" "java.lang.Class" :public ((col :int))
		    (class.forname
		     (resultsetmetadata.getcolumnclassname 
		      (MyTableModel0.rsMetaData this) (1+ col))))
       
  ("isCellEditable" :boolean :public ((row :int) (col :int)) t)

;  ("noSuchMethod" "void" :public ((row :int) (col :int)))

  ("setValueAt" "void" (:public :synchronized) ((value object.) (row :int) (col :int))
		(format t "Setting value at (~d, ~d) to ~s.~%" row col value)
		(let ((result-set (MyTableModel0.rs this)))
		  (resultset.absolute result-set (1+ row))
		  (resultset.updateobject result-set (1+ col) value)
		  (resultset.updaterow result-set))
		(abstracttablemodel.firetablecellupdated this row col)))

 (("conn" connection. :public)
  ("stmt" statement. :public)
  ("pstmt" preparedstatement. :public)
  ("rs" resultset. :public)
  ("rsMetaData" resultsetmetadata. :public)
  ("noOfColumns" :int :public)))

(defun create-and-show-gui (query)
  (jframe.setdefaultlookandfeeldecorated t)
  (let* ((panel (jpanel.new (gridlayout.new 1 0)))
	 (table
	  (jtable.new (mytablemodel0.new query)))
	    
	  (scroll-pane (jscrollpane.new table))
	  (frame (jframe.new "TableDemo"))
	  (jframe.setdefaultcloseoperation frame *jframe.exit_on_close*))
    (jpanel.add panel scroll-pane)
    (jpanel.setopaque panel t)
    (jframe.setcontentpane frame panel)
    (jframe.pack frame)
    (jframe.setvisible frame t)))

