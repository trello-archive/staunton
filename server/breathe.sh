set -e
pushd $(dirname $0)
cabal run
popd
