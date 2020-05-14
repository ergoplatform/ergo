#!/bin/bash
# setting up gpg2 for reading passphrase from parameters
# via https://github.com/beautiful-scala/scalastyle/blob/master/.github/workflows/release.yml#L16
# from https://github.com/olafurpg/sbt-ci-release/issues/95

mkdir ~/.gnupg && chmod 700 ~/.gnupg
echo use-agent >> ~/.gnupg/gpg.conf
echo pinentry-mode loopback >> ~/.gnupg/gpg.conf
echo allow-loopback-pinentry >> ~/.gnupg/gpg-agent.conf
chmod 600 ~/.gnupg/*
echo RELOADAGENT | gpg-connect-agent

# decode key
openssl aes-256-cbc -K $encrypted_394e5a7bea7c_key -iv $encrypted_394e5a7bea7c_iv -in ci/secring.asc.enc -out ci/secring.asc -d
# import key
gpg --no-tty --batch --yes --import ci/secring.asc

# publish
sbt +ergoWallet/publishSigned sonatypeBundleRelease

