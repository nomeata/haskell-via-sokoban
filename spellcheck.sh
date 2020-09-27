set -e

function check () {
	aspell -l "$1" -p ./wordlist -c "$2"
}
for file in ./??-*.md ./??ex-*.md;
do
  check en_US "$file"
done
