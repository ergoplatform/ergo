import os
import tarfile
import urllib.request
import zipfile
import shutil
import logging
import subprocess
from multiprocessing.pool import ThreadPool
from itertools import repeat

MICROSOFT_URL = "https://aka.ms/download-jdk/microsoft-jdk-21.0.1-"
JRE_SUBFOLDER = "jdk-21.0.1+12"
JDKS = {
  "windows-x64": "windows-x64.zip",
  "linux-x64": "linux-x64.tar.gz",
  "macos-x64": "macos-x64.tar.gz",
  "windows-aarch64": "windows-aarch64.zip",
  "linux-aarch64": "linux-aarch64.tar.gz",
  "macos-aarch64": "macos-aarch64.tar.gz",
}

# Specific to ergo-node .jap, collected with jdeps
JLINK_MODULES = "java.base,java.compiler,java.desktop,java.management,java.naming,java.rmi,java.sql,jdk.unsupported"

MAIN_JRE = os.environ.get("ERGO_RELEASE_PLATFORM") 
VERSION = os.environ.get("ERGO_RELEASE_TAG")

JAR_FILENAME = f"ergo-{VERSION}.jar"
SCRIPT_LOGO = f"""

             .-*%@#+-.                                                    
        .-+#@@%*=-=*%@@#+-.                                               
       +@@#+-         :=*@@+                                              
      -@@:   .........   :@@-                                             
     .@@-    #@@@@@@@%    -@@:   .@@@@@@@-:@@@@@%*.  .+%@@@%+.  .+%@@@%+. 
     %@+     .*@@*.        =@@.  :@@:.... :@@:..+@@ =@@=. .=*= -@@=. .=@@=
    #@#        .%@@=        *@%  :@@%%%%%.:@@+++%@# @@= .+++++ %@+     =@@
    =@@.      .*@@=        .@@=  :@@-:::: :@@+#@@-  #@# .==*@@.#@#     *@%
     *@%     =@@@*+++=     %@*   :@@*****::@@. =@@: .#@@*+*%@%  *@@*+*%@#.
      #@#    +#######*    *@%     =======..==   :==   .-=++-.    .-=+=-.  
       %@*.             .+@@.                                             
       .#@@@#+-.   .-+#@@%*:                       Node version: {VERSION}                    
          .-+#@@@#%@@#+-.                                                                                            


"""
def download_jdk(url):
  final_url = MICROSOFT_URL + url
  logging.warning(f"Downloading {final_url}")
  urllib.request.urlretrieve(final_url, url)

def unarchive_jdk(filename, directory):
  logging.warning(f"Extracting {filename} into {directory}")
  if filename.endswith("tar.gz"):
    tar = tarfile.open(filename, "r:gz")
    tar.extractall(directory)
    tar.close()
  elif filename.endswith("zip"):
    with zipfile.ZipFile(filename, 'r') as zip_ref:
      zip_ref.extractall(directory)

def create_jre(jlink, target_jre_dir, out_jre_dir):
   subprocess.run([
       jlink,
       "--module-path",
       os.path.join(target_jre_dir, "jmods"),
       "--add-modules",
       JLINK_MODULES,
       "--strip-debug",
       "--compress", "2",
       "--no-header-files",
       "--no-man-pages",
       "--output",
       out_jre_dir
   ], check=True)

def make_windows(target):
    app_dir = f"{target}/ergo-node"
    os.makedirs(app_dir, exist_ok=True)
    app_script = f"""    
Write-Host @"

{SCRIPT_LOGO}

"@ -ForegroundColor Red
jre/bin/java -jar {JAR_FILENAME}
    """
    with open(f"{app_dir}/ergo-node.ps1", "w") as f:
       f.write(app_script)
    
    shutil.copytree(f"{target}/jre", f"{app_dir}/jre")
    shutil.copyfile(JAR_FILENAME, f"{app_dir}/{JAR_FILENAME}")
    shutil.make_archive(f"ergo-node-{VERSION}-{target}", 'zip', app_dir)

def make_linux(target):
    app_dir = f"{target}/ergo-node"
    os.makedirs(app_dir, exist_ok=True)

    app_script = f"""#!/bin/env sh
echo '\033[0;31m'
cat << EOF
    
{SCRIPT_LOGO}

EOF
echo '\033[0m'
./jre/bin/java -jar {JAR_FILENAME}
exit
    """
    with open(f"{app_dir}/ergo-node.sh", "w") as f:
       f.write(app_script)
    os.chmod(f"{app_dir}/ergo-node.sh", 0o755)
    
    shutil.copytree(f"{target}/jre", f"{app_dir}/jre")
    shutil.copyfile(JAR_FILENAME, f"{app_dir}/{JAR_FILENAME}")
    with tarfile.open(f"ergo-node-{VERSION}-{target}.tar.gz", "w:gz") as tar:
       tar.add(app_dir)

def make_macos(target):
    app_dir = f"{target}/ErgoNode.app"
    os.makedirs(app_dir, exist_ok=True)
    os.makedirs(f"{app_dir}/Contents/MacOS", exist_ok=True)
    os.makedirs(f"{app_dir}/Contents/Resources", exist_ok=True)

    info_plist = f"""<?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
     <key>CFBundleExecutable</key>
     <string>start.command</string>
     <key>CFBundleIdentifier</key>
     <string>org.ergoplatform.ergo-node</string>
     <key>CFBundleName</key>
     <string>Ergo Node</string>
     <key>CFBundleIconFile</key>
     <string>ergo.icns</string>
     <key>CFBundleVersion</key>
     <string>{VERSION}</string>
    </dict>
    </plist>
    """
    with open(f"{app_dir}/Contents/info.plist", "w") as f:
       f.write(info_plist)

    app_script = f"""#!/bin/zsh
SRC=$(cd "$(dirname "$0")"; pwd -P)
echo '\033[0;31m'
cat << EOF

{SCRIPT_LOGO}

EOF
echo '\033[0m'
$SRC/jre/bin/java -jar $SRC/{JAR_FILENAME}

exit
    """
    app_script_file = "ergo-node.sh"
    with open(f"{app_dir}/Contents/MacOS/{app_script_file}", "w") as f:
       f.write(app_script)

    # Require nested script for macos Terminal app to open
    start_command = f"""#!/bin/zsh
MY_PATH=$(cd "$(dirname "$0")"; pwd -P)
open -a Terminal $MY_PATH/{app_script_file}
    """
    start_command_file = "start.command"
    with open(f"{app_dir}/Contents/MacOS/{start_command_file}", "w") as f:
       f.write(start_command)
    os.chmod(f"{app_dir}/Contents/MacOS/{app_script_file}", 0o755)
    os.chmod(f"{app_dir}/Contents/MacOS/{start_command_file}", 0o755)

    shutil.copytree(f"{target}/jre", f"{app_dir}/Contents/MacOS/jre")
    shutil.copyfile(JAR_FILENAME, f"{app_dir}/Contents/MacOS/{JAR_FILENAME}")
    shutil.copyfile("ci/ergo.icns", f"{app_dir}/Contents/Resources/ergo.icns")

    with tarfile.open(f"ergo-node-{VERSION}-{target}.tar.gz", "w:gz") as tar:
       tar.add(app_dir)

def process_download(entry):
  (os_type, filename) = entry
  download_jdk(filename)
  unarchive_jdk(filename, os_type)
    
def process_jres(os_type, main_jre):
  logging.warning(f"Creating jre for {os_type}")
  if (os_type.startswith("macos")):
        create_jre(main_jre, os.path.join(os_type, JRE_SUBFOLDER, "Contents", "Home"), os_type + "/jre")
        make_macos(os_type)
  if (os_type.startswith("linux")):
        create_jre(main_jre, os.path.join(os_type, JRE_SUBFOLDER), os_type + "/jre")
        make_linux(os_type)
  if (os_type.startswith("windows")):
        create_jre(main_jre, os.path.join(os_type, JRE_SUBFOLDER), os_type + "/jre")
        make_windows(os_type)
        
def get_main_jre(jre, subfolder) -> str:
    if (jre.startswith("windows")):
        return os.path.join(jre, subfolder, "bin", "jlink.exe")
    elif (jre.startswith("macos")):
        return os.path.join(jre, subfolder, "Contents", "Home", "bin", "jlink")
    else: #linux
        return os.path.join(jre, subfolder, "bin", "jlink")

logging.warning(f"Starting release binaries for ergo-node")

main_jre = get_main_jre(MAIN_JRE, JRE_SUBFOLDER)

with ThreadPool(JDKS.__len__()) as pool:
  pool.map(process_download, JDKS.items())

with ThreadPool(JDKS.__len__()) as pool:
  pool.starmap(process_jres, zip(JDKS.keys(), repeat(main_jre)))
