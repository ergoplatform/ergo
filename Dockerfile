FROM openjdk:jre-slim as builder
RUN echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list && \
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    sudo apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends \
            ca-certificates \
            curl \
            sbt
COPY . /ergo
WORKDIR /ergo
RUN sbt assembly
CMD ["/usr/bin/java", "-jar", "ergo-assembly-0.1.0.jar"]

FROM openjdk:jre-alpine
MAINTAINER Andrey Andreev <andyceo@yandex.ru> (@andyceo)
COPY --from=builder /ergo/ergo-assembly-0.1.0.jar /ergo.jar
COPY --from=builder /ergo/src/main/resources/node1/application.conf /application.conf
EXPOSE 6869 6868
VOLUME ["/root"]
ENTRYPOINT ["/usr/bin/java", "-jar", "/ergo.jar"]
CMD ["application.conf"]
