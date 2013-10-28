;; (load "/home/pjb/works/gsharp/src/abnotation/loader.lisp")
(in-package "COMMON-LISP-USER")

(import 'ccl:getenv)
(defsetf getenv ccl:setenv)



#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(setf *print-right-margin* 110)


#+ccl (setf (logical-pathname-translations "CCL")
            (cons  (list "CCL:*.pathname-translations.*"
                         (merge-pathnames
                          (make-pathname :defaults (user-homedir-pathname)
                                         :directory '(:relative "LOGHOSTS")
                                         :name :wild
                                         :type :unspecific
                                         :version :wild)
                          (user-homedir-pathname)
                          nil))
                   (logical-pathname-translations "CCL")))

;; #+ccl (translate-logical-pathname #P"ccl:PATCHWORK.pathname-translations.newest")
;; --> #P"/Users/pjb/LOGHOSTS/PATCHWORK"

(load-logical-pathname-translations "GSHARP")

(defparameter *src*
  #+ccl   #P"GSHARP:src;"
  #+clisp #P"GSHARP:SRC;")

#+ccl       (ccl::cd (truename #P"GSHARP:src;abnotation;"))
#+clisp     (ext:cd  (truename #P"GSHARP:SRC;ABNOTATION;"))
#-(or ccl clisp) (error "(cd #P\"GSHARP:SRC;ABNOTATION;\")")


(dolist (dir (find-asdf-subdirectories  (list *src*)))
  (unless (member "old" (pathname-directory dir) :test (function string=))
    (pushnew dir asdf:*central-registry* :test (function equalp))))

(ql:quickload :gsharp)
(ql:quickload :abnotation)

(load #+(or allegro ccl) #P"GSHARP:src;abnotation;gsharp-init.lisp"
      #-(or allegro ccl) #P"GSHARP:SRC;ABNOTATION;GSHARP-INIT.LISP")



(defun run-on-display (&optional (display (getenv "DISPLAY")))
  (setf (getenv "DISPLAY") display)
  (gsharp:gsharp :new-process t))

#||
(run-on-display ":0.0")
(run-on-display "kuiper.lan.informatimago.com")
||#


;; (mapcar (function pathname-name) (directory #P"GSHARP:**;*.asd"))
;; ("gsharp" "functional-geometry" "scigraph" "automaton" "esa" "mcclim-freetype" "mcclim-truetype" "mcclim-tree-with-cross-edges" "conditional-commands" "clim-examples" "clim-listener" "clouseau" "mcclim-gif-bitmaps" "mcclim-jpeg-bitmaps" "mcclim-png-bitmaps" "mcclim-tiff-bitmaps" "mcclim")
;; (ql:quickload :scigraph)




#|

;Compiler warnings for "home:works;gsharp;src;gsharp;drawing.lisp.newest" :
;   In gsharp-drawing::final-absolute-dot-xoffset: Undefined function gsharp-measure:final-relative-dot-xoffset
;Compiler warnings for "home:works;gsharp;src;gsharp;buffer.lisp.newest" :
;   In gsharp-buffer::add-new-selection: Undefined function (setf gsharp-buffer::buffer-back-selection)
;   In gsharp-buffer::add-new-selection: Undefined function gsharp-buffer::buffer-back-selection
;   In gsharp-buffer::add-new-selection: Undefined function (setf gsharp-buffer::buffer-back-selection)
;   In gsharp-buffer::add-new-selection: Undefined function gsharp-buffer::buffer-back-selection
;   In gsharp-buffer::add-new-selection: Undefined function gsharp-buffer::buffer-forward-selection
;   In gsharp-buffer::selection-browse-forward: Undefined function gsharp-buffer::buffer-forward-selection
;   In gsharp-buffer::selection-browse-forward: Undefined function (setf gsharp-buffer::buffer-forward-selection)
;   In gsharp-buffer::selection-browse-forward: Undefined function (setf gsharp-buffer::buffer-back-selection)
;   In gsharp-buffer::selection-browse-forward: Undefined function gsharp-buffer::buffer-back-selection
;   In gsharp-buffer::selection-browse-forward: Undefined function gsharp-buffer::buffer-forward-selection
;   In gsharp-buffer::selection-browse-forward: Undefined function gsharp-buffer::buffer-forward-selection
;   In gsharp-buffer::selection-browse-backward: Undefined function gsharp-buffer::buffer-back-selection
;   In gsharp-buffer::selection-browse-backward: Undefined function (setf gsharp-buffer::buffer-back-selection)
;   In gsharp-buffer::selection-browse-backward: Undefined function (setf gsharp-buffer::buffer-forward-selection)
;   In gsharp-buffer::selection-browse-backward: Undefined function gsharp-buffer::buffer-forward-selection
;   In gsharp-buffer::selection-browse-backward: Undefined function gsharp-buffer::buffer-back-selection
;   In gsharp-buffer::selection-browse-backward: Undefined function gsharp-buffer::buffer-back-selection
;   In gsharp-buffer::buffer-selection: Undefined function gsharp-buffer::buffer-back-selection
;   In gsharp-buffer::buffer-selection: Undefined function gsharp-buffer::buffer-back-selection
;Compiler warnings for "home:works;gsharp;src;mcclim;Drei;syntax.lisp.newest" :
;   In (drei-syntax:update-lex (drei-syntax:incremental-lexer t t)) inside an anonymous lambda form: Undefined function drei-syntax::low-mark
#P"/home/pjb/works/gsharp/src/loader.lisp"
cl-user> 
|#
