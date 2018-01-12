;;;; g_text.lsp
;;;; General commands for texts.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Creation date: 2011-09-07
;;;; Edition date: 2018-01-12
;;;; License: OpenSource - The MIT License (MIT)


;;;; Set up commands for text (and blocks, partially) editing.


;;; ---- COMMANDS ----

;;; COMMAND: Aligh texts and blocks

(defun c:ae (/ ang entlist entname entname_internal entname_last msg pt1 sel)
  (prompt "\nAE - Alinha texto, bloco sem atributo e atributo com objeto linear")
  (md:startcmd)

  ;; Point for alignment angle
  (setvar "osmode" 512)			; nea
  (if
    (setq
      pt1
       (getpoint
	 "\nClique numa linha para obter um ângulo de alinhamento, ou no vazio para zero graus: "
       )
    )
     (progn

       ;; Alignment angle
       (if
	 (null (setq ang (md:angle_pt pt1))) ; check if on empty space
	  (setq ang 0.00)		; set alignment angle to zero
       )
       (prompt (strcat "\nÂngulo obtido: " (angtos ang 0 2) "°."))

       ;; Entity to be aligned
       (setq msg "\nClique sobre um texto, bloco sem atributo ou atributo para alinhar: ")
       (while
	 (setq sel (entsel msg))	; selection

	  ;; Check entity
	  (setq entname (car sel))
	  (if
	    (member (cdr (assoc 0 (entget entname)))
		    '("TEXT" "MTEXT" "ATTDEF" "INSERT")

	    )				; valid entity types

	     ;; then: valid, proceed...
	     (progn

	       ;; Entity list
	       (setq entname_internal (car (nentselp (cadr sel))))
					; try to get the subentity by selection point
	       (if
		 (= (cdr (assoc 0 (entget entname_internal))) "ATTRIB")
					; subentity is a block attribute
		  (setq entlist (entget entname_internal)) ; then: from subentity
		  (setq entlist (entget entname)) ; else: from entity
	       )

	       ;; Rotate object if clicking again
	       (if (eq entname entname_last) ; if it is the same entity as the previous
		 (setq ang (md:fixangle (+ ang pi))) ; add 1 pi to the rotation angle
	       )

	       ;; Modify the entity
	       (entmod (subst (cons 50 ang) (assoc 50 entlist) entlist))
	       (entupd entname)		; update entity

	       ;; Record last entity and change message for new selections
	       (setq entname_last entname
		     msg	  "\nClique sobre um texto, bloco sem atributo ou atributo para alinhar (clique no mesmo para inverter): "
	       )
	     )

	     ;; else: invalid, prompt the user.
	     (prompt "\nElemento inválido!")
	  )
       )
     )
  )

  (md:endcmd)
  (princ)
)


;;; COMMAND: Unite texts

(defun c:unt (/ elem_1 elem_2 entlist_1 entlist_2 str_1 str_2 str_new str_new_dxf)
  (prompt "\nUNT - Unir textos")
  (md:startcmd)

  ;; element 1
  (setvar "osmode" 0)
  (setq elem_1 (car (entsel "\nSelecione o texto a ser completado: ")))

  ;; text
  (while
    (not
      (null
	(setq elem_2 (car (entsel "\nSelecione o texto a adicionar: ")))
      )
    )
     (progn
       ;; anchor text
       (setq entlist_1 (entget elem_1))
       (setq str_1 (cdr (assoc 1 entlist_1)))

       ;; complementary text
       (setq entlist_2 (entget elem_2))
       (setq str_2 (cdr (assoc 1 entlist_2)))

       ;; new text
       (setq str_new (strcat str_1 " " str_2))

       ;; replace and update
       (setq str_new_dxf (cons 1 str_new))
       (entmod
	 (subst str_new_dxf (assoc 1 entlist_1) entlist_1)
       )
     )
  )

  (md:endcmd)
  (princ)
)


;;; COMMAND: Copy and transfer text values
;;; 	TO-DO: detect Multitext vs Dtext

(defun c:tt (/ entlist2 entname1 entname1_assoc1 entname2 entname2_assoc1 mod)
  (prompt "\nTT - Transfere texto de textos, multitextos e atributos de blocos")
  (md:startcmd)

  ;; user input
  (while
    (setq entname1 (car (nentsel "\nSelecione o objeto de origem ou <Sair>: ")))

     (setq entname2 (car (nentsel "\nSelecione o objeto de destino ")))

     ;; get data
     (setq entname1_assoc1 (assoc 1 (cdr (entget entname1))))
     (setq entlist2 (entget entname2))
     (setq entname2_assoc1 (assoc 1 entlist2))

     ;; replace
     (entmod (subst entname1_assoc1 entname2_assoc1 entlist2))
  )

  (md:endcmd)
  (princ)
)


;;; COMMAND: Text numbering
;;; 	TO-DO: replace 1st while function with something simpler.

(defun c:ns (/ entlist entlist_assoc1 entname num str str_assoc1 str_num str_prefix str_sufix)
  (prompt "\nNS - Numerador sequencial")
  (md:startcmd)

  ;; user input
  (while (not (numberp
		(setq num (getint "\nDigite o número de partida: "))
	      )
	 )
    (prompt "\nIsto não é um número!")
  )
  (setq	str_prefix (getstring "\nDigite o prefixo ou <ESPAÇO> para nada: ")
	str_sufix  (getstring "\nDigite o sufixo ou <ESPAÇO> para nada: ")
  )

  ;; user selection
  (while
    (setq
      entname (car (nentsel "\nSelecione um texto ou <ENTER> para concluir: "))
    )

     ;; get data
     (setq entlist (entget entname))
     (setq entlist_assoc1 (assoc 1 entlist))

     ;; construct text and pair, then insert and modify
     (setq str_num (itoa num)
	   str	   (strcat str_prefix str_num str_sufix)
     )
     (setq str_assoc1 (cons 1 str))

     (entmod (subst str_assoc1 entlist_assoc1 entlist))
     (setq num (1+ num))
  )

  (md:endcmd)
  (princ)
)


;;; EOF