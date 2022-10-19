FROM mozilla/sbt:11.0.13_1.6.2 as builder
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
RUN mv `find target/scala-*/stripped/ -name ergo-*.jar` ergo.jar
FROM openjdk:11-oraclelinux8
RUN microdnf --nodocs -y upgrade
RUN microdnf --nodocs -y install --setopt=install_weak_deps=0 jq curl
RUN microdnf clean all
RUN adduser --home-dir /home/ergo --uid 9052 ergo && \
    install -m 0750 -o ergo -g ergo -d /home/ergo/.ergo
USER ergo
EXPOSE 9020 9021 9052 9030 9053
WORKDIR /home/ergo
VOLUME ["/home/ergo/.ergo"]
ENV MAX_HEAP 3G
ENV _JAVA_OPTIONS "-Xms${MAX_HEAP} -Xmx${MAX_HEAP}"
COPY --from=builder /mnt/ergo.jar /home/ergo/ergo.jar
ENTRYPOINT ["java", "-jar", "/home/ergo/ergo.jar"]
