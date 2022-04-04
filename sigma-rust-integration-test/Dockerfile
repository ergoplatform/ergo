FROM ergoplatform/ergo:latest
COPY ergo.conf /etc/myergo.conf
ENTRYPOINT ["java", "-jar", "/home/ergo/ergo.jar", "--devnet", "-c", "/etc/myergo.conf"]