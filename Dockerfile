FROM debian:buster AS builder

ARG DEBIAN_FRONTEND=noninteractive

# install dependencies
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
        curl \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        autoconf \
        automake \
        build-essential


# install ghcup
RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ARG GHC=9.6.2
ARG CABAL=3.10.1.0

# install GHC and cabal
RUN ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}

RUN cabal update

WORKDIR /app

COPY ./peli.cabal /app/peli.cabal
COPY ./cabal.project.freeze /app/cabal.project.freeze
COPY ./lib /app/lib
RUN cabal build --only-dependencies exe:peli-server

COPY . /app
RUN cabal build -v exe:peli-server

FROM debian:buster

WORKDIR /app

COPY --from=builder /app/dist-newstyle/build/x86_64-linux/ghc-9.6.2/peli-0.1.0.0/x/peli-server/build/peli-server/peli-server /app/peli-server

ENV PORT=8080
EXPOSE 8080

CMD ["/app/peli-server"]
