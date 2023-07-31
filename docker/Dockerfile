# docker build -t zig-master ./docker --build-arg ZIGVER=master
# docker run -it -v $(pwd):/code zig-master

FROM alpine:3.13 as builder

RUN apk update && \
	apk add \
		curl \
		xz \
		tar \
		python3

ARG ZIGVER
RUN mkdir -p /deps
WORKDIR /deps
COPY get_url.py .

RUN curl "$(python3 get_url.py $ZIGVER $(uname -m)-linux)" -o zig.tar.xz;
RUN tar xf zig.tar.xz
RUN mv *zig*/ local

#################

FROM alpine

COPY --from=builder /deps/local/ /deps/local/
ENV PATH="/deps/local:$PATH"
# RUN apk --no-cache add \
#       libc-dev \
#       xz \
#       samurai \
#       git \
#       cmake \
#       py3-pip \
#       perl-utils \
#       jq \
#       curl && \
#     pip3 install s3cmd
WORKDIR /code
