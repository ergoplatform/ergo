#!/usr/bin/env sh

# Initialize all the option variables.
# This ensures we are not contaminated by variables from the environment.
APP_DIR=~/Ergo
NODE_NAME=${USER}-$(hostname)
API_KEY=
INSTALLATION_RECOMMENDATIONS=
USE_TOR=no
USE_DAEMON=no
CONFIG_TEMPLATE="ergo.node.mining = false\nscorex.network.nodeName = \"<NODE_NAME>\"\nscorex.restApi.apiKeyHash = \"<API_KEY_HASH>\""

while :; do
    case $1 in
        -h|-\?|--help)
            echo "Usage: $0 --api-key=YOUR_API_KEY [--node-name=YOUR_NODE_NAME] [--app-dir=APP_DIR] [--daemon] [--tor]"
            exit
            ;;

        --api-key)
            if [ "$2" ]; then
                API_KEY=$2
                shift
            else
                echo 'ERROR: "--api-key" requires a non-empty option argument.'
                exit 1
            fi
            ;;
        --api-key=?*)
            API_KEY=${1#*=} # Delete everything up to "=" and assign the remainder.
            ;;
        --api-key=)         # Handle the case of an empty --api-key=
            echo 'ERROR: "--api-key" requires a non-empty option argument.'
            exit 1
            ;;

        --app-dir)
            if [ "$2" ]; then
                APP_DIR=$2
                shift
            else
                echo 'ERROR: "--app-dir" requires a non-empty option argument.'
                exit 1
            fi
            ;;
        --app-dir=?*)
            NODE_NAME=${1#*=} # Delete everything up to "=" and assign the remainder.
            ;;
        --app-dir=)         # Handle the case of an empty --app-dir=
            echo 'ERROR: "--app-dir" requires a non-empty option argument.'
            exit 1
            ;;

        --node-name)
            if [ "$2" ]; then
                NODE_NAME=$2
                shift
            else
                echo 'ERROR: "--node-name" requires a non-empty option argument.'
                exit 1
            fi
            ;;
        --node-name=?*)
            NODE_NAME=${1#*=} # Delete everything up to "=" and assign the remainder.
            ;;
        --node-name=)         # Handle the case of an empty --node-name=
            echo 'ERROR: "--node-name" requires a non-empty option argument.'
            exit 1
            ;;

        --daemon)
            USE_DAEMON=yes
            ;;

        --tor)
            USE_TOR=yes
            ;;

        --)              # End of all options.
            shift
            break
            ;;
        -?*)
            printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2
            ;;
        *)               # Default case: No more options, so break out of the loop.
            break
    esac

    shift
done

if [ -z "${API_KEY}" ]; then
  echo 'ERROR: "--api-key" is requiered and requires a non-empty option argument.'
  exit 1
fi

echo "Ergo node with config file will be installed into '${APP_DIR}' directory and will be named as '${NODE_NAME}' and has API key '${API_KEY}'."


## Check preinstalled software
INSTALLATION_RECOMMENDATIONS=
which java > /dev/null
if [ $? != 0 ]; then
  INSTALLATION_RECOMMENDATIONS="${INSTALLATION_RECOMMENDATIONS}\nsudo apt install default-jre-headless"
fi

which curl > /dev/null
if [ $? != 0 ]; then
  INSTALLATION_RECOMMENDATIONS="${INSTALLATION_RECOMMENDATIONS}\nsudo apt install curl"
fi

which daemon > /dev/null
if [ $? != 0 ]; then
  INSTALLATION_RECOMMENDATIONS="${INSTALLATION_RECOMMENDATIONS}\nsudo apt install daemon"
fi

if [ "${USE_TOR}" != "no" ]; then
  which tor > /dev/null
  if [ $? != 0 ]; then
    INSTALLATION_RECOMMENDATIONS="${INSTALLATION_RECOMMENDATIONS}\nsudo apt install tor"
  fi
fi

if [ -n "${INSTALLATION_RECOMMENDATIONS}" ]; then
  echo "To run Ergo node as you desired, you first need to install required software. For Ubuntu, run:${INSTALLATION_RECOMMENDATIONS}"
  echo "After software installed, just run this script again with same parameters"
  exit 1
fi

curl --output /dev/null --silent --head --fail http://localhost:9053
if [ "$?" = 0 ]; then
  echo "Seems node is already running on your machine (port 9053 is occupied), abort!"
  exit 1
fi

# Create APP_DIR
if [ -d ${APP_DIR} ]; then
  echo "Ergo directory '${APP_DIR}' already exists."
else
  mkdir -p ${APP_DIR}
  echo "Ergo directory '${APP_DIR}' created."
fi

# Download jar files from github
LATEST_ERGO_RELEASE=$(curl --silent "https://api.github.com/repos/ergoplatform/ergo/releases/latest" | grep -Po '"tag_name": "\K.*?(?=")')
LATEST_ERGO_RELEASE_NUMBERS=$(echo ${LATEST_ERGO_RELEASE} | cut -c 2-)
ERGO_DOWNLOAD_URL=https://github.com/ergoplatform/ergo/releases/download/${LATEST_ERGO_RELEASE}/ergo-${LATEST_ERGO_RELEASE_NUMBERS}.jar
echo "Latest known Ergo release: ${LATEST_ERGO_RELEASE}, downloading it to ${APP_DIR}/ergo.jar with overwrite..."
#curl --silent -L ${ERGO_DOWNLOAD_URL} --output ${APP_DIR}/ergo.jar
echo "Ergo was downloaded to ${APP_DIR}/ergo.jar"

# First run of Ergo node, to create API key hash that later be added into config
echo -n "Executing ergo node to obtain API key hash..."
daemon --chdir=${APP_DIR} -- java -jar ${APP_DIR}/ergo.jar --mainnet
PID=$(pgrep -f "daemon --chdir=${APP_DIR} -- java")
while ! curl --output /dev/null --silent --head --fail http://localhost:9053; do sleep 1 && echo -n '.'; done;
API_KEY_HASH=$(curl -X POST "http://localhost:9053/utils/hash/blake2b" -H  "accept: application/json" -H  "Content-Type: application/json" -d "\"${APP_KEY}\"" --silent | cut -c 2- | rev | cut -c 2- | rev)
echo "\nAPI key hash obtained: ${API_KEY_HASH}"
echo -n "Stopping Ergo node with PID=${PID}..."
kill ${PID} && while kill -0 ${PID} > /dev/null 2>&1; do sleep 1 && echo -n '.'; done;
echo "\nStopped."


# Writing config file
echo ${CONFIG_TEMPLATE} | sed "s/<NODE_NAME>/${NODE_NAME}/g" | sed "s/<API_KEY_HASH>/${API_KEY_HASH}/g" > ${APP_DIR}/application.conf
echo "Config file writed to ${APP_DIR}/application.conf"

# Start node with config
echo -n "Starting node..."
daemon --chdir=${APP_DIR} -- java -jar ${APP_DIR}/ergo.jar --mainnet -c ${APP_DIR}/application.conf
PID=$(pgrep -f "daemon --chdir=${APP_DIR} -- java")
while ! curl --output /dev/null --silent --head --fail http://localhost:9053; do sleep 1 && echo -n '.'; done;
echo "\nNode started."
echo "To stop the node later, use: kill ${PID}"
