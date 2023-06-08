!include "MUI2.nsh"
!include "StrFunc.nsh"
${Using:StrFunc} StrStr
${Using:StrFunc} UnStrStr
${Using:StrFunc} UnStrRep

Name "Elixir"
ManifestDPIAware true
Unicode True
InstallDir "$PROGRAMFILES64\Elixir"
!define MUI_ICON "assets\drop.ico"

; Install Page: Install Erlang/OTP

Page custom InstallOTPPageShow InstallOTPPageLeave

var Dialog
var InstallOTPCheckbox
Function InstallOTPPageShow
  !insertmacro MUI_HEADER_TEXT "Install Erlang/OTP" ""

  nsDialogs::Create 1018
  Pop $Dialog

  ${If} $Dialog == error
    Abort
  ${EndIf}

  EnumRegKey $0 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang" 0
  ReadRegStr $0 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang\$0" ""

  ${NSD_CreateCheckbox} 0 0 195u 10u "&Install included Erlang/OTP ${OTP_VERSION}"
  Pop $InstallOTPCheckbox

  ${If} $0 == ""
    SendMessage $InstallOTPCheckbox ${BM_SETCHECK} ${BST_CHECKED} 0
  ${EndIf}

  nsDialogs::Show
FunctionEnd

Function InstallOTPPageLeave
  ${NSD_GetState} $InstallOTPCheckbox $0
  ${If} $0 <> ${BST_UNCHECKED}
    CreateDirectory "$INSTDIR\tmp"
    SetOutPath "$INSTDIR\tmp"

    File "${OTP_INSTALLER}"
    ExecWait '"$INSTDIR\${OTP_INSTALLER}"'
    Delete "$INSTDIR\${OTP_INSTALLER}"
  ${EndIf}
FunctionEnd

; Install Page: Files

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

; Install Page: Finish

Page custom FinishPageShow FinishPageLeave

var AddOTPToPathCheckbox
var AddElixirToPathCheckbox
var OTPPath
Function FinishPageShow
  !insertmacro MUI_HEADER_TEXT "Finish Setup" ""

  nsDialogs::Create 1018
  Pop $Dialog

  ${If} $Dialog == error
    Abort
  ${EndIf}

  ${NSD_CreateCheckbox} 0 0 195u 10u "&Add $INSTDIR\bin to %PATH%"
  Pop $AddElixirToPathCheckbox
  SendMessage $AddElixirToPathCheckbox ${BM_SETCHECK} ${BST_CHECKED} 0

  EnumRegKey $0 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang" 0
  ReadRegStr $0 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang\$0" ""
  StrCpy $OTPPath $0
  ${If} $0 != ""
    ${NSD_CreateCheckbox} 0 20u 195u 10u "&Add $0\bin to %PATH%"
    Pop $AddOTPToPathCheckbox
    SendMessage $AddOTPToPathCheckbox ${BM_SETCHECK} ${BST_CHECKED} 0
  ${EndIf}

  nsDialogs::Show
FunctionEnd

Function FinishPageLeave
  ${NSD_GetState} $AddOTPToPathCheckbox $0
  ${If} $0 <> ${BST_UNCHECKED}
    ReadRegStr $0 HKCU "Environment" "Path"
    ${StrStr} $1 "$0" "$OTPPath\bin"
    ${If} $1 == ""
      WriteRegExpandStr HKCU "Environment" "Path" "$OTPPath\bin;$0"
    ${Else}
      MessageBox MB_OK "$OTPPath\bin already in %PATH%"
    ${EndIf}
  ${EndIf}

  ${NSD_GetState} $AddElixirToPathCheckbox $0
  ${If} $0 <> ${BST_UNCHECKED}
    ReadRegStr $0 HKCU "Environment" "Path"
    ${StrStr} $1 "$0" "$INSTDIR\bin"
    ${If} $1 == ""
      WriteRegExpandStr HKCU "Environment" "Path" "$INSTDIR\bin;$0"
    ${Else}
      MessageBox MB_OK "$INSTDIR\bin already in %PATH%"
    ${EndIf}
  ${EndIf}
FunctionEnd

Section "Install Elixir" SectionElixir
  SetOutPath "$INSTDIR"
  File /r "${ELIXIR_DIR}\"

  WriteUninstaller "Uninstall.exe"
SectionEnd

; Uninstall Page: Files

!insertmacro MUI_UNPAGE_DIRECTORY
!insertmacro MUI_UNPAGE_INSTFILES

Section "Uninstall"
  RMDir /r "$INSTDIR"
SectionEnd

; Uninstall Page: Finish

var RemoveOTPFromPathCheckbox
var RemoveElixirFromPathCheckbox
Function un.FinishPageShow
  !insertmacro MUI_HEADER_TEXT "Finish Setup" ""

  nsDialogs::Create 1018
  Pop $Dialog

  ${If} $Dialog == error
    Abort
  ${EndIf}

  ReadRegStr $0 HKCU "Environment" "Path"
  ${UnStrStr} $1 "$0" "$INSTDIR\bin"
  ${If} $1 != ""
    ${NSD_CreateCheckbox} 0 0 195u 10u "&Remove $INSTDIR\bin from %PATH%"
    Pop $RemoveElixirFromPathCheckbox
    SendMessage $RemoveElixirFromPathCheckbox ${BM_SETCHECK} ${BST_CHECKED} 0
  ${EndIf}

  EnumRegKey $1 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang" 0
  ReadRegStr $1 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang\$1" ""
  StrCpy $OTPPath $1
  ${UnStrStr} $1 "$0" "$OTPPath\bin"
  ${If} $1 != ""
    ${NSD_CreateCheckbox} 0 20u 195u 10u "&Remove $OTPPath\bin from %PATH%"
    Pop $RemoveOTPFromPathCheckbox
    SendMessage $RemoveOTPFromPathCheckbox ${BM_SETCHECK} ${BST_CHECKED} 0
  ${EndIf}

  nsDialogs::Show
FunctionEnd

Function un.FinishPageLeave
  ReadRegStr $0 HKCU "Environment" "Path"

  ${NSD_GetState} $RemoveOTPFromPathCheckbox $1
  ${If} $1 <> ${BST_UNCHECKED}
    ${UnStrRep} $0 "$0" "$OTPPath\bin;" ""
  ${EndIf}

  ${NSD_GetState} $RemoveElixirFromPathCheckbox $1
  ${If} $1 <> ${BST_UNCHECKED}
    ${UnStrRep} $0 "$0" "$INSTDIR\bin;" ""
  ${EndIf}

  WriteRegExpandStr HKCU "Environment" "Path" "$0"
FunctionEnd

UninstPage custom un.FinishPageShow un.FinishPageLeave

!insertmacro MUI_LANGUAGE "English"
