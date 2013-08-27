;;;; c_config.lsp
;;;; Config manager
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2013.08.27
;;;; Licence: Creative Commons BY-NC-SA 3.0 BR
;;;; http://www.creativecommons.org.br
;;;; Require: DOSLib

;;;; This manages the configuration file. Access, read, write. Opens UI.

;;; Notes:
;;;	1. (nothing)

;;; TO-DO:
;;;	1) (nothing)

;;; Index:
;;;	CFG
;;;		MD_CONFIGLOAD - MultiDEV configuration/initialization load (ini file)


;;; ==== CFG  ====

;;; MD_CONFIGLOAD - MultiDEV configuration/initialization load (ini file)

;; Syntax
;;	(md_configload)
;; Parameters
;;	none
;; Returns
;;	the path and file name of loaded INI file if succesful
;;	kill on error
;; Operation
;;	The config is recorded in gv:md_settings ('nil' on load), variable created and resetable by md_settings at a_multidev.lsp.
;; Example
;;	(md_configload)

(defun md_configload (/ *key *pathfilename *section entrylist)

  ;; Find and load CFG
  (prompt "\nCarregando configuração... ")			      ; prompt user
  (if
    (findfile (setq *pathfilename (strcat gv:multidevpath "\\" gv:md_cfgfile)))
								      ; find the INI file hardcoded in a_multidev.lsp

     ;; then: get data from the INI file
     (progn

       ;; Collect and record per-key setting
       (setq gv:showsplashscreen
	      (dos_getini "startup" "showsplashscreen" *pathfilename) ; show splashscreen
	     gv:loadmenu
	      (dos_getini "interface" "loadmultidevmenu" *pathfilename) ; load MultiDEV menu
	     gv:menufile
	      (dos_getini "interface" "multidevmenufile" *pathfilename) ; MultiDEV menu file name
	     gv:dwgscale
	      (atof (dos_getini "drawing" "scale" *pathfilename))     ; base scale for drawing
	     gv:dwgunit
	      (atoi (dos_getini "drawing" "unit" *pathfilename))      ; base unit for drawing
       ) ;_ setq

       ;; Build an assoc list of the settings
       (setq *section  "commands"				      ; section of CFG
	     entrylist (dos_getini *section nil *pathfilename)	      ; list all entries within section
       ) ;_ setq
       (setq gv:md_settings
	      (mapcar '(lambda (*key)				      ; record gv:md_settings
			 (cons *key (dos_getini *section *key *pathfilename))
		       ) ;_ lambda
		      entrylist
	      ) ;_ mapcar
       ) ;_ setq
       (prompt "OK!")						      ; prompt user
       *pathfilename						      ; returns the path if successful
     ) ;_ progn

     ;; else: prompt user and exit
     (progn
       (prompt
	 (strcat "\nErro: arquivo de configuração " gv:md_cfgfile " não encontrado! Abortando...")
       ) ;_  prompt
       (exit)							      ; kill
     ) ;_ progn
  ) ;_ if
) ;_ defun

(md_configload)


;;;;; blocks
;;;(dos_username) ; get user name
;;;(dos_filep) ; file exists?
;;;(dos_getdir ...) ; opens a 'select a folder' dialog
;;;(dos_getini "secao1" "key1" (findfile "MultiDEV.ini")) ; read
;;;(dos_setini "secao1" "key2" "alterei2" (findfile "MultiDEV.ini")) ; overwrite
;;;(dos_setini "secaonova" "key1_new" "novo_$" (findfile "MultiDEV.ini")) ; write new

;;;(md_readcfg "C:\\config.cfg" "keyword1")


;;;; LOAD LAYER CFGs
;;; KEY_NAME '("Layer name" cor layltype)

;;;; LOAD LAYER

;;; EOF