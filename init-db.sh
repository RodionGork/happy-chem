#set -x
HOST=http://localhost:18080

while read p; do
  [ -z "$p" ] && continue
  PARAMS=$(echo "$HOST/molecule/$p" | sed -r 's/(.*)\,(.*)\,(.*)/-d \"\3 \2\" \1/')
  printf "$p - "
  eval "curl -X POST ""$PARAMS"
done <molecules.txt

while read p; do
  [ -z "$p" ] && continue
  PARAMS=$(echo "$HOST/catalyst/$p" | sed -r 's/(.*)\,(.*)\,(.*)/-d \"\3 \2\" \1/')
  printf "$p - "
  eval "curl -X POST ""$PARAMS"
done <catalysts.txt
