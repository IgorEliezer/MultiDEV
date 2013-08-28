;;;; b_startup.lsp
;;;; Load libraries and configurations. Splashscreen.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2011.11.20
;;;; Licence: Creative Commons BY-NC-SA 3.0 BR
;;;; http://www.creativecommons.org.br
;;;; Requires: DOSLib

;;;; This loads the libraries and configurations.
;;;; Once MultiDEV is found and system folders are set, let's load stuff up!
;;;; This exhibits MultiDEV splashscreen.

;;; Notes:
;;;	1. (nothing)

;;; TO-DO:
;;;	1) Create (findfile_key [cfg] <key> <filename> <ext>)
;;;	2) Create an INI editor (another LSP file)
;;;	3) Integration with OpenDCL (future)
;;;	4) ESSENTIAL: Check AutoCAD Version!!!

;;; Index:
;;;	LIBS
;;;		MD_LOADDOSLIB - DOSLib loader according to AutoCAD version and system's processor architecture
;;;	INTERFACE
;;;		MD_MENULOAD - MultiDEV menuload
;;;		MD_SPLASHSCREEN - MultiDEV splashscreen


;;; ==== LIBS  ====

;;; MD_LOADDOSLIB - DOSLib loader

;; Syntax
;;	(md_loaddoslib)
;; Parameters
;;	none
;; Returns
;;	the path and file name of loaded ARX if succesful
;;	kill on error
;; Operation
;;	DOSLib loader according to AutoCAD version and system's processor architecture.
;; Example
;;	(md_loaddoslib)

(defun md_loaddoslib (/ acadvernum filename pathfilename)

  ;; Build file name and path according to AutoCAD version and system's processor architecture
  (setq acadvernum (substr (getvar "ACADVER") 1 2))		      ; AutoCAD version
  (if
    (= "AMD64" (getenv "PROCESSOR_ARCHITECTURE"))		      ; processor architecture
     (setq filename (strcat "DOSLib" acadvernum "x64.arx"))	      ; DOSLib for AMD 64 bits
     (setq filename (strcat "DOSLib" acadvernum ".arx"))	      ; DOSLib for others
  ) ;_ if
  (setq pathfilename (strcat gv:dospath "\\" filename))		      ; build full path

  ;; Check if required DOSLib is already loaded
  (if
    (member (strcase filename t) (arx))				      ; check if the file, in lowercase, is in the list of loaded ARXs

     ;; then: prompt and do nothing else
     (progn
       (prompt (strcat "\nBiblioteca " filename " já está carregada."))
       pathfilename						      ; returns
     ) ;_  progn

     ;; else: find and load DOSLib
     (progn
       (prompt (strcat "\nLocalizando biblioteca " filename "... "))  ; prompt user
       (if
	 (findfile pathfilename)				      ; find required file

	  ;; then: load DOSLib
	  (arxload pathfilename nil)				      ; load and returns the path

	  ;; else: alert the user and exit
	  (progn
	    (prompt (strcat "\nErro: " filename " não encontrado! Abortando...")) ; prompt the user
	    (exit)						      ; kill
	  ) ;_  progn
       ) ;_  if
     ) ;_  progn
  ) ;_  if
) ;_  defun

(md_loaddoslib)							      ; execute


;;; ==== INTERFACE ====

;;; MD_MENULOAD - MultiDEV menuload

;; Syntax
;;	(md_menuload)
;; Parameters
;;	none
;; Returns
;;	the path and file name of loaded CUI file if succesful
;;	nil on error
;; Operation
;;	Loads the menu file according to CAD version and INI settings.
;; Example
;;	(md_menuload)

;; Notes:
;; 	1) According to the AutoCAD Help, CUI was used in ACAD 2006-2009 (v17), and CUIx from 2010 on (v18-v19). MNS and
;;		  MNU are prior to ACAD 2006 and, therefore, they are unsupported by MultiDEV.

;; --- A bit about AutoCAD version number stored in ACADVER system variable
;; Version number differs from release number. There is a release number per year, while version number is bound to file
;;	format and it's returned by ACADVER system variable which is used by MultiDEV to load external libraries.

;; Official name	Version	Release
;; AutoCAD 2004 	16.0 	18
;; AutoCAD 2005 	16.1 	19
;; AutoCAD 2006 	16.2 	20
;; AutoCAD 2007 	17.0 	21
;; AutoCAD 2008 	17.1 	22
;; AutoCAD 2009 	17.2 	23
;; AutoCAD 2010 	18.0 	24
;; AutoCAD 2011 	18.1 	25
;; AutoCAD 2012 	18.2 	26
;; AutoCAD 2013 	19.0 	27

(defun md_menuload (/ AcadVerNum CmdEcho_or FileExt FileName GroupName PathFileName)

  ;; Check INI
  (if
    (= (dos_getini "interface" "loadmultidevmenu" gv:md_cfgfile) "yes")
								      ; menu load is enabled by INI, section "interface"

     ;; then: find and load menu MultiDEV
     (progn
       (prompt "\nCarregando interface... ")			      ; prompt user

       ;; Define if either CUI or CUIx file will be loaded, according to AutoCAD version number
       (setq AcadVerNum (substr (getvar "ACADVER") 1 2))	      ; AutoCAD version
       (cond
	 ((= (atoi AcadVerNum) 17)				      ; ACAD 2007 through 2009: CUI
	  (setq FileExt ".cui")
	 )
	 ((>= (atoi AcadVerNum) 18)				      ; ACAD 2010 or higher: CUIx
	  (setq FileExt ".cuix")
	 )
	 (t nil)
       ) ;_ cond

       ;; File name and menugroup name
       (setq GroupName (dos_getini "interface" "multidevmenufile" gv:md_cfgfile)
								      ; menugroup from INI
	     FileName  (strcat GroupName FileExt)		      ; file name, same as menugroup
       ) ;_ setq

       ;; Find and load menu file
       (if
	 (findfile (setq PathFileName (strcat gv:guipath "\\" FileName))) ; find cui file

	  ;; then: if found, load menugroup
	  (progn

	    ;; Command echoing
	    (setq CmdEcho_or (getvar "CMDECHO"))		      ; get original echoing value
	    (setvar "CMDECHO" 0)				      ; turn off echoing

	    ;; Unload menugroup if already loaded
	    (if
	      (menugroup GroupName)				      ; if loaded
	       (command "_cuiunload" GroupName)			      ; unload
	    ) ;_ if

	    ;; Load menugroup
	    (command "_cuiload" PathFileName)			      ; load cui file
	    (menucmd (strcat "P16=+" GroupName ".POP1"))	      ; and set up the menu
	    (prompt "Menu MultiDEV carregado.")			      ; prompt user

	    ;; Restore command echoing
	    (setvar "CMDECHO" CmdEcho_or)			      ; restore original echoing value

	    ;; Return
	    PathFileName					      ; returns the path if successful
	  ) ;_ progn

	  ;; else: if not found, prompt user, but do not exit.
	  (progn
	    (prompt
	      (strcat "Alerta: arquivo de customização de interface "
		      FileName
		      " não encontrado! Prosseguindo..."
	      ) ;_  strcat
	    ) ;_ prompt
	    nil							      ; returns nil, if failed
	  ) ;_ progn
       ) ;_ if
     ) ;_ progn
  ) ;_ if
) ;_ defun

(md_menuload)							      ; execute


;;; MD_SPLASHSCREEN - MultiDEV splashscreen

;; Syntax
;;	(md_splashscreen)
;; Parameters
;;	none
;; Returns
;;	the path and file name of displayed splashscreen file.
;;	nil on error
;; Operation
;;	If the program has survived until now, show splashscreen
;;	
;; Example
;;	(md_splashscreen)

(defun md_splashscreen (/ filename pathfilename)

  ;; Check INI
  (if
    (= (dos_getini "startup" "showsplashscreen" gv:md_cfgfile) "yes") ; splashscreen is enabled by INI, section "startup"

     ;; Find and show splashscreen
     (progn
       (setq filename "splash.png")				      ; png file (must be hardcoded)
       (if
	 (findfile (setq pathfilename (strcat gv:guipath "\\" filename))) ; find the png file

	  ;; then: if found, show splashscreen
	  (progn
	    (dos_splash pathfilename 5 t)			      ; show for 3 seconds and allow user to dismiss it
	    pathfilename					      ; returns the path if successful
	  ) ;_ progn

	  ;; else: if not found, prompt user, but do not exit.
	  (progn
	    (prompt
	      (strcat "\nErro: arquivo de splashscreen " filename " não encontrado! Prosseguindo...")
	    ) ;_  prompt
	    nil							      ; returns nil if failed
	  ) ;_ progn
       ) ;_ if
     ) ;_ if
  ) ;_ if
) ;_ defun

(md_splashscreen)   ; execution


;; MultiDEV is ready to load!...
(prompt "\nDEBUG: Avançando para o módulo seguinte...")

;;; EOF
