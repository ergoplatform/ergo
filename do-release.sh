#!/bin/bash

set -euo pipefail

required_java_version="1.8"
java_version=$(java -version 2>&1 | awk -F '"' '/version/ {print $2}')

if [[ $java_version != $required_java_version* ]]; then
  echo "Incompatible Java version: $java_version. Requires 1.8.x."
  exit 1
fi

rm -rf target
sbt clean assembly

jar=$(find "target/scala-2.12" -name 'ergo-*.jar' -print -quit)

if [[ -z $jar ]]; then
  echo "Ergo jar not found."
  exit 1
fi

jar_version=$(echo "$jar" | sed 's/^.*ergo-\(.*\)\.jar$/\1/')
conf_file="src/main/resources/application.conf"
conf_version=$(grep "appVersion =" "$conf_file" | cut -d '=' -f2 | tr -d '[:space:]')

if [[ -z $conf_version ]]; then
  echo "Version not found in application.conf."
  exit 1
fi

if [[ $conf_version != $jar_version ]]; then
  echo "Version mismatch: application.conf ($conf_version) != jar ($jar_version)."
  echo "Removing jar $jar"
  rm "$jar"
  exit 1
fi

openapi_files=("openapi.yaml" "openapi-ai.yaml")

for file in "${openapi_files[@]}"; do
  openapi_path="src/main/resources/api/$file"
  version_line=$(grep 'version:' "$openapi_path")

  if [[ -z $version_line ]]; then
    echo "Error: Version line not found in $openapi_path"
    exit 1
  fi

  actual_version=$(echo $version_line | awk -F '"' '{print $2}')

  if [[ -z $actual_version ]]; then
    echo "Error: Version not found in $openapi_path"
    exit 1
  fi

  if [[ $actual_version != $jar_version ]]; then
    echo "Version mismatch in $openapi_path: ($actual_version) != jar ($jar_version)."
    echo "Removing jar $jar"
    rm "$jar"
    exit 1
  fi
done

echo "do-release completed successfully"
