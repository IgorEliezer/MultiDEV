;;;; a_multidev.lsp
;;;; MultiDEV boot!
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Creation date: 2011-11-20
;;;; Edition date: 2018-01-11
;;;; License: OpenSource - The MIT License (MIT)


;;;; First find MultiDEV and set the path.


;;;; /!\ MULTIDEV REQUIRES AutoCAD 2007 OR HIGHER /!\
;;;; Custom functions and global variables are prefixed with "md:"


;;; ---- HELLO WORLD! ----

(prompt "\n\nCarregando MultiDEV... ")	; prompt loading


;;; FUNCTION: Set MultiDEV version

(defun md:version ()
  (setq *md:ver* "0.X.X ____, YYYY-MM-DD") ; version number, stage and date
)

(md:version)				; execute


;;; ---- COMPABILITY ----

;;; FUNCTION: Get CAD version and check if it supports MultiDEV
;;;	BricsCAD or AutoCAD version 17 or higher.

(defun md:cadsupport (/ acadvernum)
  (prompt "\nVerificando compatibilidade com o AutoCAD... ")
  (setq acadvernum (substr (getvar "ACADVER") 1 2))
  (if
    (< (atoi acadvernum) 17)		; if earlier than AutoCAD 2007, v17

     ;; then: alert the user and exit
     (progn
       (alert
	 "A versão de AutoCAD que você está usando não é compatível com MultiDEV!\n
O MultiDEV requer AutoCAD 2007 ou superior para funcionar."
       )
       (prompt
	 "Erro: Versão do AutoCAD incompatível com MultiDEV! Carregamento cancelado."
       )
       (exit)
     )

     ;; else: continue
     (progn
       (prompt "OK.")
       t
     )
  )
)

(md:cadsupport)				; execute


;;; ---- INITIAL SETTINGS ----

;;; FUNCTION: Working MultiDEV path
;;;	Find MultiDEV.vlx in the search path and, if successful, record it in a global variable.
;;;	Otherwise, warn the user to add MultiDEV in the search path.
;;;	Test: (findfile "MultiDEV.vlx")

(defun md:path (/ file filepath)
  (setq file "MultiDEV.vlx")
  (if
    (setq filepath (findfile file))

     ;; then: record MultiDEV path and return it
     (setq *md:path* (substr filepath 1 (- (strlen filepath) 13)))

     ;; else: alert the user and exit
     (progn
       (alert
	 "O MultiDEV não foi adicionado na busca de arquivos de suporte do AutoCAD!\n\nSiga os passos:
- dê o comando _OPTIONS;
- selecione aba/seção 'Files';
- selecione 'Support File Search Path';
- clique no botão 'Add..' e em 'Browse...', localize a pasta do MultiDEV, selecione-a e clique em 'OK';
- aplique as alterações clicando em 'Apply';
- recarregue o MultiDEV."
       )
       (prompt
	 (strcat "\nErro: A pasta do "
		 file
		 " não foi encontrada. Carregamento cancelado!"
	 )
       )
       (setq *md:path* nil)
       (exit)
     )
  )
)

(md:path)				; execute


;; it's all set! Now go ahead to start MultiDEV up...
;;;(prompt "\nDEBUG: Avançando para o módulo seguinte...")


;;; EOF

