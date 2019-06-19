FROM openjdk:11-jre-slim as builder
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends apt-transport-https apt-utils bc dirmngr gnupg && \
    echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    # seems that dash package upgrade is broken in Debian, so we hold it's version before update
    echo "dash hold" | dpkg --set-selections && \
    apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends sbt
COPY ["build.sbt", "lock.sbt", "/ergo/"]
COPY ["project", "/ergo/project"]
RUN sbt update
COPY . /ergo
WORKDIR /ergo
RUN sbt assembly
RUN mv `find . -name ergo-*.jar` /ergo.jar
CMD ["java", "-jar", "/ergo.jar"]

FROM openjdk:11-jre-slim
LABEL maintainer="Andrey Andreev <andyceo@yandex.ru> (@andyceo)"
RUN adduser --disabled-password --home /home/ergo --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo -d /home/ergo/.ergo
COPY --from=builder /ergo.jar /home/ergo/ergo.jar
USER ergo
EXPOSE 9020 9052
WORKDIR /home/ergo
VOLUME ["/home/ergo/.ergo"]
ENTRYPOINT ["java", "-jar", "/home/ergo/ergo.jar"]
CMD [""]
