;;;; a_multidev.lsp
;;;; MultiDEV boot!
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2011.11.20
;;;; Licence: Creative-commons, non-profit unless by given authorization
;;;; http://www.creativecommons.org.br

;;;; This lists all global variables and their default values. All global variables must start with 'gv:'.
;;;; First find MultiDEV and set the hardcoded directories. Then, the initial settings.

;;;; /!\ MULTIDEV REQUIRES AUTOCAD 2007 OR HIGHER /!\

;;; Notes:
;;;	1. (nothing)

;;; TO-DO:
;;;	1) Create (findfile_key [cfg] <key> <filename> <ext>)
;;;	2) gv:pause pause ; record pause variable (TO-DO: check if it's possible to replace pause with "\\")
;;;	3) gv:dwgscale should go to the modules.
;;;	4) (md_directories): check if the folder exists.

;;; Index:
;;;	HELLO WORLD!
;;;	AUTOCAD COMPABILITY
;;;	INITIAL SETTINGS
;;;		MD_FOLDER - MultiDEV hardcoded folder system
;;;		MD_SETTINGS - MultiDEV initial settings
;;;	EXECUTION


;;; ==== HELLO WORLD! ====

(prompt "\n\nCarregando MultiDEV... ")				      ; prompt loading


;;; ==== COMPABILITY ====

;;; MD_CADSUPPORT - Check if CAD version supports MultiDEV

;; Syntax
;;	(md_cadsupport)
;; Parameters
;;	none
;; Returns
;;	T if successful
;;	nil on error.
;; Operation
;;	Check CAD version and if it supports MultiDEV, which requires CAD version 17 or higher.
;; Example
;;	(md_cadsupport)


(defun md_cadsupport (/ acadvernum)
  (prompt "\nVerificando compatibilidade com o CAD... ")	      ; prompt user
  (setq acadvernum (substr (getvar "ACADVER") 1 2))		      ; get CAD version
  (if
    (< (atoi acadvernum) 17)					      ; older than AutoCAD 2007, version 17

     ;; then: alert the user and exit
     (progn
       (alert
	 "A versão de AutoCAD que você está usando não é compatível com MultiDEV!\n
O MultiDEV requer AutoCAD 2007 ou superior para funcionar."
       )							      ; prompt user
       (prompt
	 "Erro: Versão do AutoCAD incompatível com MultiDEV! Carregamento cancelado."
       )							      ; prompt user
       (exit)							      ; kill
     ) ;_ progn

     ;; else: continue
     (progn
       (prompt "OK!")						      ; prompt user
       t							      ; returns T if successful
     ) ;_ progn
  ) ;_ if
) ;_ defun

(md_cadsupport)							      ; execute


;;; ==== INITIAL SETTINGS ====

;;; MD_DIRECTORIES - MultiDEV hardcoded directory system

;; Syntax
;;	(md_directories)
;; Parameters
;;	none
;; Returns
;;	T if successful
;;	exit on error.
;; Operation
;;	First, find MultiDEV.vlx, once found, find the MD directories and record them.
;;	NOTICE! The directories here must match the ones in the installation package or it will not work!
;; Example
;;	(md_directories)

;; Notes
;;	1) Probably an INI file would work better here...

(defun md_directories (/ multidev_path)
  (if
    (setq multidev_path (findfile "MultiDEV.vlx"))		      ; find MultiDEV.vlx

     ;; then: make paths
     (progn

       ;; MultiDEV root
       (setq multidev_path
	      (substr multidev_path
		      1
		      (- (strlen multidev_path) 13)
	      ) ;_ substr
       )							      ; remove file name and the last '\\'

       ;; set global variables for MultiDEV root and directories
       (setq gv:multidevpath multidev_path			      ; MultiDEV root
	     gv:addpath	     (strcat multidev_path "\\" "addons")     ; addons/plug-ins directory
	     gv:cfgpath	     (strcat multidev_path "\\" "config")     ; configs directory
	     gv:datpath	     (strcat multidev_path "\\" "dats")	      ; data files directory
	     gv:dospath	     (strcat multidev_path "\\" "doslib")     ; DOSLib directory
	     gv:docpath	     (strcat multidev_path "\\" "help")	      ; help files directory
	     gv:guipath	     (strcat multidev_path "\\" "interface")  ; user interface directory
	     gv:lngpath	     (strcat multidev_path "\\" "language")   ; language files
	     gv:libpath	     (strcat multidev_path "\\" "library")    ; resource library directory
       ) ;_ setq

       ;; check if directories are not found
       (prompt "\nLocalizando pastas requeridas... ")		      ; prompt user

       (if (member nil						      ; if one of the paths in the lit is not found
		   (mapcar 'findfile
			   (list gv:addpath	 gv:cfgpath	 gv:datpath	 gv:dospath
				 gv:docpath	 gv:guipath	 gv:lngpath	 gv:libpath
				) ;_ list
 ;_ list
		   ) ;_ mapcar
	   ) ;_ member

	 ;; then: alert the user and exit
	 (progn
	   (prompt
	     "Erro: Nem todas as pastas foram encontradas! Carregamento cancelado."
	   )							      ; prompt user
	   (exit)						      ; kill
	 ) ;_ progn

	 ;; else: continue
	 (progn
	   (prompt "OK!")					      ; prompt user
	   t							      ; returns T if successful
	 ) ;_ progn
       ) ;_ if
     ) ;_ progn

     ;; else: alert the user and exit
     (progn
       (alert
	 "O MultiDEV não foi adicionado na busca de arquivos de suporte do AutoCAD!\n\nSiga os seguintes passos:
- dê o comando _OPTIONS;
- aba 'Files';
- duplo-clique em 'Support File Search Path'; 
- clique no botão 'Add..' e em 'Browse...', localize a pasta do MultiDEV, selecione-a e clique em 'OK';
- aplique as alterações clicando em 'Apply';
- recarregue o MultiDEV."
       ) ;_ alert
       (prompt "\nCarregamento cancelado!")			      ; prompt user
       (exit)							      ; kill
     ) ;_ progn
  ) ;_ if
) ;_ defun

(md_directories)						      ; execute


;;; MD_SETTINGS - MultiDEV default values settings

;; Syntax
;;	(md_settings)
;; Parameters
;;	none
;; Returns
;;	T if successful
;;	nil on error.
;; Operation
;;	Define MultiDEV default values settings.
;; Example
;;	(md_settings)

;; Notes
;;	1) Probably it'll need not to reset values while debugging.
;;	2) TIP: use this function to reset the settings.

(defun md_settings ()

  ;; set global variables for MultiDEV settings
  (prompt "\nDefinindo configuração padrão... ")		      ; prompt user

  (setq	gv:md_version	 "0.9.0 ??.Ago.2013 HH:MM"		      ; version number and build time						     
	gv:md_cfgfile	 "MultiDEV.ini"				      ; config/ini
  ) ;_ setq

  (prompt "OK!")						      ; prompt user
  t								      ; returns T if successful
) ;_ defun

(md_settings)							      ; execute


;; it's all set! Now go ahead to start MultiDEV up and running...
(prompt "\nDEBUG: Avançando para o módulo seguinte...")

;;; EOF