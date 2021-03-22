!define Version "%VERSION%"

; The name of the installer
Name "Jailer ${Version}"

; The file to write
OutFile "Jailer-Install-${Version}.exe"

; The default installation directory
InstallDir $PROGRAMFILES\Jailer

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\Jailer" "Install_Dir"

; Request application privileges for Windows Vista
RequestExecutionLevel admin

;--------------------------------

; Pages

Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

Icon "..\src\main\gui\net\sf\jailer\ui\resource\jailer.ico"

;--------------------------------

; The stuff to install
Section "Jailer"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Put file there

  File /r "C:\tmp\jailer\*.*"
  
  FileOpen $4 ".singleuser" w
  FileWrite $4 "1"
  FileClose $4

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\Jailer "Install_Dir" "$INSTDIR"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Jailer" "DisplayName" "Jailer"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Jailer" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Jailer" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Jailer" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

  CreateDirectory "$SMPROGRAMS\Jailer"
  Delete "$SMPROGRAMS\Jailer\*.*"
  CreateShortcut "$SMPROGRAMS\Jailer\Jailer ${Version}  .lnk" "$INSTDIR\Jailer.exe" 
  CreateShortcut "$SMPROGRAMS\Jailer\Jailer ${Version} Data Browser.lnk" "$INSTDIR\JailerDataBrowser.exe" 
;  CreateShortcut "$SMPROGRAMS\Jailer\Uninstall.lnk" "$INSTDIR\uninstall.exe" 
  
SectionEnd

Section "Desktop Shortcuts"

  CreateShortcut "$DESKTOP\Jailer.lnk" "$INSTDIR\Jailer.exe" 
  CreateShortcut "$DESKTOP\Jailer Data Browser.lnk" "$INSTDIR\JailerDataBrowser.exe" 
  
SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Jailer"
  DeleteRegKey HKLM SOFTWARE\Jailer

  ; Remove files and uninstaller
;  Delete $INSTDIR\example2.nsi
;  Delete $INSTDIR\uninstall.exe

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\Jailer\*.*"
  Delete "$INSTDIR\*.*"

  Delete "$DESKTOP\Jailer ${Version}.lnk"
  Delete "$DESKTOP\Jailer ${Version} Data Browser.lnk"

  ; Remove directories used
  RMDir "$SMPROGRAMS\Jailer"
  RMDir /r "$INSTDIR"

SectionEnd
