pwd
# init git
mkdir -p /tmp/ocaml-matrix/
cd /tmp/ocaml-matrix/
git init
touch root
git add root
git commit -m "init"
cd /var/lib/matrix-server/

# generate key
openssl genpkey -algorithm Ed25519 -out /var/lib/matrix-server/key

# run
matrix-ci-server-bin $SERVER_NAME foo,/var/lib/matrix-server/key
