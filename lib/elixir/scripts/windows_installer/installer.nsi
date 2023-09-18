!include "MUI2.nsh"
!include "StrFunc.nsh"
${Using:StrFunc} UnStrStr

Name "Elixir"
ManifestDPIAware true
Unicode True
InstallDir "$PROGRAMFILES64\Elixir"
!define MUI_ICON "assets\drop.ico"

; Install Page: Install Erlang/OTP

Page custom CheckOTPPageShow CheckOTPPageLeave

var Dialog
var DownloadOTPLink
var InstalledOTPRelease
var OTPPath
var OTPVerified
Function CheckOTPPageShow
  !insertmacro MUI_HEADER_TEXT "Checking Erlang/OTP" ""

  nsDialogs::Create 1018
  Pop $Dialog

  ${If} $Dialog == error
    Abort
  ${EndIf}

  StrCpy $0 0
  loop:
    EnumRegKey $1 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang" $0
    StrCmp $1 "" done
    ReadRegStr $1 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang\$1" ""
    StrCpy $OTPPath $1
    IntOp $0 $0 + 1
    goto loop
  done:

  ${If} $OTPPath == ""
    ${NSD_CreateLabel} 0 0   100% 20u "Couldn't find existing Erlang/OTP installation. Click the link below to download and install it before proceeding."
    ${NSD_CreateLink}  0 25u 100% 20u "Download Erlang/OTP ${OTP_RELEASE}"
    Pop $DownloadOTPLink
    ${NSD_OnClick} $DownloadOTPLink OpenOTPDownloads
  ${Else}
    nsExec::ExecToStack `$OTPPath\bin\erl.exe -noinput -eval "\
    io:put_chars(erlang:system_info(otp_release)),\
    halt()."`
    Pop $0
    Pop $1

    ${If} $0 == 0
      StrCpy $InstalledOTPRelease $1
      ${If} $InstalledOTPRelease == ${OTP_RELEASE}
        ${NSD_CreateLabel} 0 0 100% 20u "Found existing Erlang/OTP $InstalledOTPRelease installation at $OTPPath. Please proceed."
        StrCpy $OTPVerified "true"
      ${ElseIf} $2 < ${OTP_RELEASE}
        ${NSD_CreateLabel} 0 0 100% 30u "Found existing Erlang/OTP $InstalledOTPRelease installation at $OTPPath but this Elixir installer was precompiled for Erlang/OTP ${OTP_RELEASE}. \
        We recommend checking if there is an Elixir version precompiled for Erlang/OTP $InstalledOTPRelease. Otherwise, proceed."
      ${Else}
        SetErrorlevel 5
        MessageBox MB_ICONSTOP "Found existing Erlang/OTP $InstalledOTPRelease installation at $OTPPath but this Elixir version was precompiled for Erlang/OTP ${OTP_RELEASE}. \
        Please upgrade your Erlang/OTP version or choose an Elixir installer matching your Erlang/OTP version"
      ${EndIf}
    ${Else}
      SetErrorlevel 5
      MessageBox MB_ICONSTOP "Found existing Erlang/OTP installation at $OTPPath but checking it exited with $0: $1"
    ${EndIf}
  ${EndIf}

  nsDialogs::Show
FunctionEnd

Function OpenOTPDownloads
  ExecShell "open" "https://www.erlang.org/downloads/${OTP_RELEASE}"
FunctionEnd

Function CheckOTPPageLeave
FunctionEnd

; Install Page: Files

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

; Install Page: Finish

Page custom FinishPageShow FinishPageLeave

var AddOTPToPathCheckbox
var AddElixirToPathCheckbox
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

  ${If} $OTPVerified == "true"
  ${NSD_CreateCheckbox} 0 20u 195u 10u "&Add $OTPPath\bin to %PATH%"
  Pop $AddOTPToPathCheckbox
  SendMessage $AddOTPToPathCheckbox ${BM_SETCHECK} ${BST_CHECKED} 0
  ${EndIf}

  ${NSD_CreateLabel} 0 40u 100% 20u "Note: you need to restart your shell for the enviornment variable changes to take effect."

  nsDialogs::Show
FunctionEnd

var PathsToAdd
Function FinishPageLeave
  ${NSD_GetState} $AddOTPToPathCheckbox $0
  ${If} $0 <> ${BST_UNCHECKED}
    StrCpy $PathsToAdd ";$OTPPath\bin"
  ${EndIf}

  ${NSD_GetState} $AddElixirToPathCheckbox $0
  ${If} $0 <> ${BST_UNCHECKED}
    StrCpy $PathsToAdd "$PathsToAdd;$INSTDIR\bin"
  ${EndIf}

  ${If} "$PathsToAdd" != ""
    nsExec::ExecToStack `"$OTPPath\bin\escript.exe" "$INSTDIR\update_system_path.erl" add "$PathsToAdd"`
    Pop $0
    Pop $1
    ${If} $0 != 0
      SetErrorlevel 5
      MessageBox MB_ICONSTOP "adding paths failed with $0: $1"
      Quit
    ${EndIf}
  ${EndIf}
FunctionEnd

Section "Install Elixir" SectionElixir
  SetOutPath "$INSTDIR"
  File /r "${ELIXIR_DIR}\"
  File "update_system_path.erl"

  WriteUninstaller "Uninstall.exe"
SectionEnd

; Uninstall Page: Remove from %PATH%

var RemoveOTPFromPathCheckbox
var RemoveElixirFromPathCheckbox
Function un.FinishPageShow
  !insertmacro MUI_HEADER_TEXT "Remove from %PATH%" ""

  StrCpy $0 0
  loop:
    EnumRegKey $1 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang" $0
    StrCmp $1 "" done
    ReadRegStr $1 HKLM "SOFTWARE\WOW6432NODE\Ericsson\Erlang\$1" ""
    StrCpy $OTPPath $1
    IntOp $0 $0 + 1
    goto loop
  done:

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

  ${UnStrStr} $1 "$0" "$OTPPath\bin"
  ${If} $1 != ""
    ${NSD_CreateCheckbox} 0 20u 195u 10u "&Remove $OTPPath\bin from %PATH%"
    Pop $RemoveOTPFromPathCheckbox
    SendMessage $RemoveOTPFromPathCheckbox ${BM_SETCHECK} ${BST_CHECKED} 0
  ${EndIf}

  nsDialogs::Show
FunctionEnd

var PathsToRemove
Function un.FinishPageLeave
  ${NSD_GetState} $RemoveOTPFromPathCheckbox $1
  ${If} $1 <> ${BST_UNCHECKED}
    StrCpy $PathsToRemove ";$OTPPath\bin"
  ${EndIf}

  ${NSD_GetState} $RemoveElixirFromPathCheckbox $1
  ${If} $1 <> ${BST_UNCHECKED}
    StrCpy $PathsToRemove "$PathsToRemove;$INSTDIR\bin"
  ${EndIf}

  ${If} "$PathsToRemove" != ""
    nsExec::ExecToStack `"$OTPPath\bin\escript.exe" "$INSTDIR\update_system_path.erl" remove "$PathsToRemove"`
    Pop $0
    Pop $1
    ${If} $0 != 0
      SetErrorlevel 5
      MessageBox MB_ICONSTOP "removing paths failed with $0: $1"
      Quit
    ${EndIf}
  ${EndIf}
FunctionEnd

UninstPage custom un.FinishPageShow un.FinishPageLeave

!insertmacro MUI_UNPAGE_DIRECTORY
!insertmacro MUI_UNPAGE_INSTFILES

Section "Uninstall"
  RMDir /r "$INSTDIR"
SectionEnd

!insertmacro MUI_LANGUAGE "English"
