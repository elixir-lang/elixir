!include "MUI2.nsh"
!include "StrFunc.nsh"
${Using:StrFunc} UnStrStr

Name "Elixir"
ManifestDPIAware true
Unicode True
InstallDir "$PROGRAMFILES64\Elixir"
!define MUI_ICON "assets\Elixir.ico"
!define MUI_UNICON "assets\Elixir.ico"

; Install Page: Install Erlang/OTP

Page custom CheckOTPPageShow CheckOTPPageLeave

var InstalledOTPRelease
var OTPPath

var Dialog
var NoOTPLabel
var NoOTPLabelCreated
var OTPMismatchLabel
var OTPMismatchLabelCreated
var DownloadOTPLink
var DownloadOTPLinkCreated
var VerifyOTPButton
var VerifyOTPButtonCreated
Function CheckOTPPageShow
  !insertmacro MUI_HEADER_TEXT "Checking Erlang/OTP" ""

  nsDialogs::Create 1018
  Pop $Dialog

  ${If} $Dialog == error
    Abort
  ${EndIf}

  Call VerifyOTP

  nsDialogs::Show
FunctionEnd

Function VerifyOTP
  ${If} $NoOTPLabelCreated == "true"
    ShowWindow $NoOTPLabel ${SW_HIDE}
  ${EndIf}

  ${If} $OTPMismatchLabelCreated == "true"
    ShowWindow $OTPMismatchLabel ${SW_HIDE}
  ${EndIf}

  ${If} $DownloadOTPLinkCreated == "true"
    ShowWindow $DownloadOTPLink ${SW_HIDE}
  ${Else}
    StrCpy $DownloadOTPLinkCreated "true"
    ${NSD_CreateLink}  0 60u 100% 20u "Download Erlang/OTP ${OTP_RELEASE}"
    Pop $DownloadOTPLink
    ${NSD_OnClick} $DownloadOTPLink OpenOTPDownloads
    ShowWindow $DownloadOTPLink ${SW_HIDE}
  ${EndIf}

  ${If} $VerifyOTPButtonCreated == "true"
    ShowWindow $VerifyOTPButton ${SW_HIDE}
  ${Else}
    StrCpy $VerifyOTPButtonCreated "true"
    ${NSD_CreateButton} 0 80u 25% 12u "Verify Erlang/OTP"
    Pop $VerifyOTPButton
    ${NSD_OnClick} $VerifyOTPButton VerifyOTP
    ShowWindow $VerifyOTPButton ${SW_HIDE}
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
    ${If} $NoOTPLabelCreated != "true"
      StrCpy $NoOTPLabelCreated "true"
      ${NSD_CreateLabel} 0 0   100% 20u "Couldn't find existing Erlang/OTP installation. Click the link below to download and install it before proceeding."
      Pop $NoOTPLabel
    ${EndIf}

    ShowWindow $NoOTPLabel ${SW_SHOW}
    ShowWindow $DownloadOTPLink ${SW_SHOW}
    ShowWindow $VerifyOTPButton ${SW_SHOW}

  ${Else}
    nsExec::ExecToStack `$OTPPath\bin\erl.exe -noinput -eval "\
    io:put_chars(erlang:system_info(otp_release)),\
    halt()."`
    Pop $0
    Pop $1

    ${If} $0 == 0
      StrCpy $InstalledOTPRelease $1
      ${If} $InstalledOTPRelease == ${OTP_RELEASE}
        ${NSD_CreateLabel} 0 0 100% 60u "Found existing Erlang/OTP $InstalledOTPRelease installation at $OTPPath. Please proceed."

      ${Else}
        ${If} $OTPMismatchLabelCreated != "true"
          StrCpy $OTPMismatchLabelCreated "true"
          ${NSD_CreateLabel} 0 0 100% 60u "Found existing Erlang/OTP $InstalledOTPRelease installation at $OTPPath but this Elixir installer was precompiled for Erlang/OTP ${OTP_RELEASE}. \
          $\r$\n$\r$\nYou can either search for another Elixir installer precompiled for Erlang/OTP $InstalledOTPRelease or download Erlang/OTP ${OTP_RELEASE} and install before proceeding."
          Pop $OTPMismatchLabel
        ${EndIf}

        ShowWindow $OTPMismatchLabel ${SW_SHOW}
        ShowWindow $DownloadOTPLink  ${SW_SHOW}
        ShowWindow $VerifyOTPButton  ${SW_SHOW}
      ${EndIf}
    ${Else}
      SetErrorlevel 5
      MessageBox MB_ICONSTOP "Found existing Erlang/OTP installation at $OTPPath but checking it exited with $0: $1"
    ${EndIf}
  ${EndIf}
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

  ; we add to PATH using erlang, so there must be an OTP installed to do so.
  ${If} "$OTPPath" != ""
    ${NSD_CreateCheckbox} 0 0 195u 10u "&Add $INSTDIR\bin to %PATH%"
    Pop $AddElixirToPathCheckbox
    SendMessage $AddElixirToPathCheckbox ${BM_SETCHECK} ${BST_CHECKED} 0

    ${NSD_CreateCheckbox} 0 20u 195u 10u "&Add $OTPPath\bin to %PATH%"
    Pop $AddOTPToPathCheckbox
    SendMessage $AddOTPToPathCheckbox ${BM_SETCHECK} ${BST_CHECKED} 0

    ${NSD_CreateLabel} 0 40u 100% 20u "Note: you need to restart your shell for the environment variable changes to take effect."
  ${EndIf}

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
  File "assets\Elixir.ico"
  File "update_system_path.erl"
  WriteRegStr   HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elixir" "DisplayName" "Elixir"
  WriteRegStr   HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elixir" "DisplayVersion" "${ELIXIR_VERSION}"
  WriteRegStr   HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elixir" "DisplayIcon" "$INSTDIR\Elixir.ico"
  WriteRegStr   HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elixir" "Publisher" "The Elixir Team"
  WriteRegStr   HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elixir" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elixir" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elixir" "NoRepair" 1

  WriteRegStr   HKLM "Software\Elixir\Elixir" "InstallRoot" "$INSTDIR"

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
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elixir"
  DeleteRegKey HKLM "Software\Elixir\Elixir"
  DeleteRegKey /ifempty HKLM "Software\Elixir"
SectionEnd

!insertmacro MUI_LANGUAGE "English"
