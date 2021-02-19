FROM mozilla/sbt:11.0.8_1.3.13 as builder
WORKDIR /mnt
COPY build.sbt findbugs-exclude.xml ./
COPY project/ project/
COPY avldb/build.sbt avldb/build.sbt
COPY avldb/project/ avldb/project/
COPY ergo-wallet/build.sbt ergo-wallet/build.sbt
COPY ergo-wallet/project/ ergo-wallet/project/
COPY benchmarks/build.sbt benchmarks/build.sbt
RUN sbt update
COPY . ./
RUN sbt assembly
RUN mv `find . -name ergo-*.jar` ergo.jar

FROM openjdk:11-jre-slim
RUN adduser --disabled-password --home /home/ergo --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo -d /home/ergo/.ergo
USER ergo
EXPOSE 9020 9052 9030 9053
WORKDIR /home/ergo
VOLUME ["/home/ergo/.ergo"]
ENV MAX_HEAP 3G
ENV _JAVA_OPTIONS "-Xmx${MAX_HEAP}"
COPY --from=builder /mnt/ergo.jar /home/ergo/ergo.jar
ENTRYPOINT ["java", "-jar", "/home/ergo/ergo.jar"]
