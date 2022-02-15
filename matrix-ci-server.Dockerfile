FROM ocaml/opam:debian-11 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev m4 pkg-config git -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin main && git reset --hard 3aeec9a7dbcf8b310f27ff8b337ae612e3b0147b && opam update
COPY --chown=opam \
	matrix-ci-server.opam \
	matrix-common.opam \
	matrix-ctos.opam \
	matrix-stos.opam \
	/src/

WORKDIR /src
RUN opam-2.1 install -y --deps-only .
ADD --chown=opam . .
RUN opam-2.1 exec -- dune subst
RUN opam-2.1 exec -- dune build ./_build/install/default/bin/matrix-ci-server-setup ./_build/install/default/bin/matrix-ci-server-bin


FROM debian:11
RUN apt-get update && apt-get install libev4 openssh-client curl gnupg2 dumb-init git ca-certificates netbase -y --no-install-recommends
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN echo 'deb [arch=amd64] https://download.docker.com/linux/debian buster stable' >> /etc/apt/sources.list
RUN apt-get update && apt-get install docker-ce -y --no-install-recommends

WORKDIR /var/lib/matrix-server
RUN git config --global user.email "matrix@localhost" && git config --global user.name "Matrix server" && git config --global init.defaultBranch main

ENTRYPOINT ["dumb-init", "/usr/local/bin/docker_entry.sh"]
ENV OCAMLRUNPARAM=a=2
COPY --from=build /src/_build/install/default/bin/matrix-ci-server-setup /src/_build/install/default/bin/matrix-ci-server-bin /usr/local/bin/
COPY docker_entry.sh /usr/local/bin/
EXPOSE 8008
EXPOSE 8448
