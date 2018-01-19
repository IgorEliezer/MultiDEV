;;;; h_help.lsp
;;;; MultiDEV's about and user help.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Creation date: 2011-09-07
;;;; Edition date: 2018-01-13
;;;; License: OpenSource - The MIT License (MIT)


;;;; Help UI.


;;; ---- COMMANDS ----

;;; AJ - Ajuda e lista de comandos do MultiDEV

(defun c:aj (/ filename filepath pathfilename)
  (prompt "\nAJ - Ajuda e lista de comandos do MultiDEV")
  (md:startcmd)

  ;; Alert the user
  (alert
    (strcat "*** MultiDEV - Aplicativo multidisciplinar para CAD ***"
	    "\nVers�o "
	    *md:ver*
	    "\n\nComandos:"
	    "\n	AE - Alinha texto, bloco sem atributo e atributo"
	    "\n	CUR - Definir camada corrente por sele��o"
	    "\n	NLA - Criar camada"
	    "\n	NS - Numerador sequencial"
	    "\n	TT - Transfere textos"
	    "\n	UNT - Unir textos"
	    "\n\nFeito para AutoCAD 2007 ou superior."
	    "\n\nEscrito por Igor Eliezer Borges (igoreliezer.com)\nLicen�a OpenSource/MIT."
	    "\n\n[Rodando AutoCAD r"
	    (substr (getvar "ACADVER") 1 4)
	    ", processador "
	    (getenv "PROCESSOR_ARCHITECTURE")
	    "]"
    )
  )

  (md:endcmd)
  (princ)
)


;;; FINAL MESSAGES

(prompt	(strcat	"\nMultiDEV carregado. Vers�o "
		*md:ver*
		". (c) Igor Eliezer Borges, licen�a OpenSource/MIT."
	)
)
(prompt "\nDigite AJ para abrir a lista de comandos.") ; tip


;; EOF


