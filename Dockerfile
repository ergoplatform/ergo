FROM mozilla/sbt:11.0.8_1.3.13 as builder
COPY ["build.sbt", "/ergo/"]
COPY ["project", "/ergo/project"]
RUN sbt -Dsbt.rootdir=true update
COPY . /ergo
WORKDIR /ergo
RUN sbt -Dsbt.rootdir=true assembly
RUN mv `find . -name ergo-*.jar` /ergo.jar
CMD ["java", "-jar", "/ergo.jar"]

FROM openjdk:11-jre-slim
RUN adduser --disabled-password --home /home/ergo --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo -d /home/ergo/.ergo
USER ergo
EXPOSE 9020 9052 9030 9053
WORKDIR /home/ergo
VOLUME ["/home/ergo/.ergo"]
ENV MAX_HEAP 3G
ENV _JAVA_OPTIONS "-Xmx${MAX_HEAP}"
COPY --from=builder /ergo.jar /home/ergo/ergo.jar
ENTRYPOINT ["java", "-jar", "/home/ergo/ergo.jar"]
