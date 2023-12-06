#!/bin/bash
# Usage:
#
# With Elixir archive:
#
#     ELIXIR_ZIP=Precompiled.zip OTP_VERSION=25.3.2.2 ./build.sh
set -euo pipefail

mkdir -p tmp
rm -rf tmp/elixir
unzip -d "tmp/elixir" "${ELIXIR_ZIP}"

elixir_version=`cat tmp/elixir/VERSION`
otp_release=`erl -noshell -eval 'io:put_chars(erlang:system_info(otp_release)), halt().'`
otp_version=`erl -noshell -eval '{ok, Vsn} = file:read_file(code:root_dir() ++ "/releases/" ++ erlang:system_info(otp_release) ++ "/OTP_VERSION"), io:put_chars(Vsn), halt().'`
elixir_exe=elixir-otp-${otp_release}.exe

# brew install makensis
# apt install -y nsis
# choco install -y nsis
export PATH="/c/Program Files (x86)/NSIS:${PATH}"
makensis \
  -X"OutFile tmp\\${elixir_exe}" \
  -DOTP_RELEASE="${otp_release}" \
  -DOTP_VERSION=${otp_version} \
  -DELIXIR_DIR=tmp\\elixir \
  -DELIXIR_VERSION=${elixir_version} \
  installer.nsi

echo "Installer path: tmp/${elixir_exe}"
