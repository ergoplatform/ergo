FROM openjdk:9-jre-slim as builder
RUN apt-get update && \
    apt-get install -y --no-install-recommends apt-transport-https apt-utils bc dirmngr gnupg && \
    echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends sbt
COPY . /ergo
WORKDIR /ergo
RUN sbt reload clean assembly
RUN mv `find . -name ergo-assembly*.jar` /ergo.jar
CMD ["/usr/bin/java", "-jar", "/ergo.jar"]

FROM openjdk:9-jre-slim
MAINTAINER Andrey Andreev <andyceo@yandex.ru> (@andyceo)
COPY --from=builder /ergo.jar /ergo.jar
EXPOSE 9001 9051
WORKDIR /root
VOLUME ["/root/ergo/data"]
ENTRYPOINT ["/usr/bin/java", "-jar", "/ergo.jar"]
CMD [""]
