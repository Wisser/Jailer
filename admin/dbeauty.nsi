!define Version "%VERSION%"

; The name of the installer
Name "DBeauty ${Version}"

; The file to write
OutFile "DBeauty-Install-${Version}.exe"

; The default installation directory
InstallDir $PROGRAMFILES\DBeauty

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\DBeauty" "Install_Dir"

; Request application privileges for Windows Vista
RequestExecutionLevel admin

;--------------------------------

; Pages

Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

Icon "..\src\main\gui\net\sf\jailer\ui\resource\Jailer.ico"

;--------------------------------

; The stuff to install
Section "DBeauty"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Put file there

  File /r "C:\tmp\dbeauty\*.*"
  
  FileOpen $4 ".singleuser" w
  FileWrite $4 "1"
  FileClose $4

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\DBeauty "Install_Dir" "$INSTDIR"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DBeauty" "DisplayName" "DBeauty"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DBeauty" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DBeauty" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DBeauty" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

  CreateDirectory "$SMPROGRAMS\DBeauty"
  Delete "$SMPROGRAMS\DBeauty\*.*"
  CreateShortcut "$SMPROGRAMS\DBeauty\DBeauty ${Version}  .lnk" "$INSTDIR\dbeauty.exe" 
  CreateShortcut "$SMPROGRAMS\DBeauty\Uninstall.lnk" "$INSTDIR\uninstall.exe" 
  
SectionEnd

Section "Desktop Shortcuts"

  CreateShortcut "$DESKTOP\DBeauty.lnk" "$INSTDIR\dbeauty.exe"  
  
SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DBeauty"
  DeleteRegKey HKLM SOFTWARE\DBeauty

  ; Remove files and uninstaller
;  Delete $INSTDIR\example2.nsi
;  Delete $INSTDIR\uninstall.exe

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\DBeauty\*.*"
  Delete "$INSTDIR\*.*"

  Delete "$DESKTOP\DBeauty ${Version}.lnk"

  ; Remove directories used
  RMDir "$SMPROGRAMS\DBeauty"
  RMDir "$INSTDIR"

SectionEnd
