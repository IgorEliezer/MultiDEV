;;;; e_error.lsp
;;;; Command error handling functions 
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Creation date: 2009 & 2011-05-08
;;;; Edition date: 2018-01-12
;;;; License: OpenSource - The MIT License (MIT)


;;;; Create a new command error handling method if user escapes a command or on error.


;;; ---- FUNCIONS ----

;;; FUNCTION: Command ini
;;; 	Saves some values for later and turns off echoing.

(defun md:startcmd ()

  ;; save values
  (setq	*md:cmdecho_or*	(getvar "CMDECHO") ; echoing
	*md:error_or*	*error*		; error handling
	*md:osmode_or*	(getvar "OSMODE")
  )

  ;; change values
  (setvar "CMDECHO" 0)
  (setq *error* md:error)		; new command error handling

  ;; create undo group
  (command "_undo" "_begin")		; avoid multiple undoes if the user regrets
  (princ)
)


;;; FUNCTION: Command end
;;; 	If command finishes or on error.

(defun md:endcmd ()
  (command "_undo" "_end")		; undo group created

  ;; restore original values
  (setvar "CMDECHO" *md:cmdecho_or*)
  (setvar "OSMODE" *md:osmode_or*)
  (setq *error* *md:error_or*)
  (princ)
)


;;; FUNCTION: Internal error handling
;;; 	Prompts error message e retores values in case of error.

(defun md:error	(msg)
  (prompt "\nComando cancelado!\n")	; message
  (md:endcmd)				; restore original command settings
)


;;; EOF