FROM debian:stretch

LABEL "name"="Build fused-effects"
LABEL "maintainer"="GitHub Actions <support+actions@github.com>"
LABEL "version"="0.1.0"

LABEL "com.github.actions.name"="build"
LABEL "com.github.actions.description"="Builds fused-effects"
LABEL "com.github.actions.icon"="git-pull-request"
LABEL "com.github.actions.color"="purple"

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH=/root/.cabal/bin:/opt/ghc/bin:$PATH

RUN apt-get update && \
    apt-get install -y \
      gnupg \
      libtinfo-dev \
      && \
    apt-get autoremove -y && \
    apt-get clean -y && \
    rm -rf /var/lib/apt/lists/*

RUN echo "deb http://downloads.haskell.org/debian stretch main" >> /etc/apt/sources.list && \
    apt-key adv --keyserver keyserver.ubuntu.com  --recv-keys BA3CBA3FFE22B574 && \
    apt-get update && \
    apt-get install -y \
      ghc-8.6.3 \
      cabal-install-2.4 && \
    apt-get autoremove -y && \
    apt-get clean -y && \
    rm -rf /var/lib/apt/lists/*

# Buggy versions of ld.bfd fail to link some Haskell packages: https://sourceware.org/bugzilla/show_bug.cgi?id=17689.
# Gold is faster anyways and uses less RAM.
RUN update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20 && \
    update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10

WORKDIR /usr/src/fused-effects

COPY fused-effects.cabal .
RUN cabal new-update
RUN cabal new-configure --enable-benchmarks --enable-tests --write-ghc-environment-files=always
RUN cabal new-build --only-dependencies

COPY . .
RUN cabal new-build \
    && cabal new-test \
    && cabal new-haddock \
    && cabal new-sdist \
    && cabal check

ENTRYPOINT ["./build.sh"]
