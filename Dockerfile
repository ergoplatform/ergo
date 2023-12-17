FROM sbtscala/scala-sbt:eclipse-temurin-11.0.15_1.7.1_2.13.8 as builder
WORKDIR /mnt
COPY build.sbt findbugs-exclude.xml ./
COPY project/ project/
COPY avldb/build.sbt avldb/build.sbt
COPY avldb/project/ avldb/project/
COPY ergo-core/build.sbt ergo-core/build.sbt
COPY ergo-wallet/build.sbt ergo-wallet/build.sbt
COPY ergo-wallet/project/ ergo-wallet/project/
RUN sbt update
COPY . ./
RUN sbt assembly
RUN mv `find target/scala-*/stripped/ -name ergo-*.jar` ergo.jar

FROM eclipse-temurin:11-jre-jammy
RUN apt-get update && apt-get install -y curl jq && rm -rf /var/lib/apt/lists/*
RUN adduser --disabled-password --home /home/ergo --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo -d /home/ergo/.ergo
USER ergo
EXPOSE 9020 9021 9022 9052 9030 9053
WORKDIR /home/ergo
VOLUME ["/home/ergo/.ergo"]
ENV MAX_HEAP 3G
ENV _JAVA_OPTIONS "-Xms${MAX_HEAP} -Xmx${MAX_HEAP}"
COPY --from=builder /mnt/ergo.jar /home/ergo/ergo.jar
ENTRYPOINT ["java", "-jar", "/home/ergo/ergo.jar"]
