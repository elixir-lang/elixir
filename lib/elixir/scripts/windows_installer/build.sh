#!/bin/bash
# Usage:
#
# With Elixir archive:
#
#     ELIXIR_ZIP=Precompiled.zip OTP_VERSION=25.3.2.2 ./build.sh
#
# With Elixir version:
#
#     ELIXIR_VERSION=1.14.5 OTP_VERSION=25.3.2.2 ./build.sh
set -euo pipefail

OTP_VERSION="${OTP_VERSION:-26.0}"
otp_release=`echo "${OTP_VERSION}" | cut -d. -f1`

mkdir -p tmp

ELIXIR_VERSION="${ELIXIR_VERSION:-}"
if [ -n "${ELIXIR_VERSION}" ]; then
  ELIXIR_ZIP="tmp/elixir-${ELIXIR_VERSION}-otp-${otp_release}.zip"
  if [ ! -f "${ELIXIR_ZIP}" ]; then
    url="https://github.com/elixir-lang/elixir/releases/download/v${ELIXIR_VERSION}/elixir-otp-${otp_release}.zip"
    echo "downloading ${url}"
    curl --fail -L -o "${ELIXIR_ZIP}" "${url}"
  fi
  basename=elixir-${ELIXIR_VERSION}-otp-${otp_release}
else
  basename=elixir-otp-${otp_release}
fi

if [ ! -d "tmp/${basename}" ]; then
  unzip -d "tmp/${basename}" "${ELIXIR_ZIP}"
fi

# brew install makensis
# apt install -y nsis
# choco install -y nsis
export PATH="/c/Program Files (x86)/NSIS:${PATH}"
makensis \
  -X"OutFile tmp\\${basename}.exe" \
  -DOTP_VERSION=${OTP_VERSION} \
  -DOTP_RELEASE="${otp_release}" \
  -DELIXIR_DIR=tmp\\${basename} \
  installer.nsi

echo "Installer path: tmp/${basename}.exe"
