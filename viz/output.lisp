;;;; viz/output.lisp --- writing figures and running viz/render.py
;;;;
;;;; Lisp never holds a Python process: EMIT-FIGURE writes the figure's JSON
;;;; and, for any output but JSON, runs viz/render.py on it as a program.

(in-package #:fincl-viz)

(defvar *display* :png
  "What EMIT-FIGURE produces from a figure NAME, beside NAME.json under
*OUTPUT-DIRECTORY*: :PNG (default), :SVG, :PDF or another image format writes
NAME.png etc.; :HTML a self-contained NAME.html; :JSON nothing more (no Python
needed); :BROWSER opens the interactive figure without blocking the REPL.")

(defvar *output-directory* #p"/tmp/fincl-viz/")

(defvar *python-command* "python3"
  "The Python interpreter that runs *RENDERER*; it must import plotly, and
kaleido for images. The fincl dev shell provides one.")

(defvar *renderer* (asdf:system-relative-pathname "fincl-viz" "viz/render.py")
  "The script that renders a figure's JSON file.")

(define-condition render-failure (error)
  ((command :initarg :command :reader render-failure-command)
   (error-output :initarg :error-output :reader render-failure-error-output))
  (:report (lambda (c s)
             (format s "Rendering failed: ~{~A~^ ~}~%~A"
                     (render-failure-command c) (render-failure-error-output c))))
  (:documentation "viz/render.py, or the interpreter running it, failed."))

(defun run-python (arguments)
  "Run *PYTHON-COMMAND* with ARGUMENTS and wait; signal RENDER-FAILURE with its
error output unless it exits with 0."
  (let ((command `(,*python-command* ,@arguments)))
    (multiple-value-bind (output error-output status)
        (handler-case (uiop:run-program command :ignore-error-status t
                                                :error-output :string)
          (error (e) (values nil (princ-to-string e) nil)))
      (declare (ignore output))
      (unless (eql status 0)
        (error 'render-failure :command command :error-output error-output)))))

(defvar *python-modules* '()
  "Modules *PYTHON-COMMAND* is known to import.")

(defun ensure-python-modules (modules)
  "Signal RENDER-FAILURE unless *PYTHON-COMMAND* imports every one of MODULES.
Without this check a missing interpreter or module surfaces as a traceback."
  (let ((missing (set-difference modules *python-modules* :test #'string=)))
    (when missing
      (handler-case (run-python `("-c" ,(format nil "import ~{~A~^, ~}" missing)))
        (render-failure (c)
          (error 'render-failure
                 :command (render-failure-command c)
                 :error-output (format nil "~A cannot import ~{~A~^, ~}. Start the Lisp from ~
the fincl dev shell (nix develop, or direnv reload).~%~A"
                                       *python-command* missing
                                       (render-failure-error-output c)))))
      (setf *python-modules* (union missing *python-modules* :test #'string=)))))

(defun render-figure (json output)
  "Run *RENDERER* on the file JSON, writing OUTPUT, or, when OUTPUT is NIL,
opening a browser without waiting for it."
  (let ((image (and output (not (member (pathname-type output) '("html" "htm")
                                        :test #'string-equal)))))
    (ensure-python-modules (if image '("plotly" "kaleido") '("plotly")))
    (let ((arguments `(,(namestring *renderer*) ,(namestring json)
                       ,@(when output `("-o" ,(namestring output))))))
      (if output
          (run-python arguments)
          (uiop:launch-program `(,*python-command* ,@arguments))))))

(defun output-path (name type)
  (merge-pathnames (make-pathname :name name :type type) *output-directory*))

(defun emit-figure (figure name &key (display *display*))
  "Write FIGURE, over *THEME*, as NAME.json under *OUTPUT-DIRECTORY* and
produce what DISPLAY asks for (see *DISPLAY*); returns the pathname of the
file produced, NAME.json for :JSON and :BROWSER.

  (emit-figure '(:data ((:type :bar :x #(\"a\" \"b\") :y #(3 5)))) \"bars\" :display :html)"
  (check-input display keyword "a keyword such as :png, :svg, :html, :json or :browser")
  (let ((json (write-figure (themed figure) (output-path name "json"))))
    (case display
      (:json json)
      (:browser (render-figure json nil) json)
      (t (let ((output (output-path name (string-downcase (symbol-name display)))))
           (render-figure json output)
           output)))))
