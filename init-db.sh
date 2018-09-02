#set -x
HOST=http://localhost:18080

printf "\nAdding molecules:\n\n"

while read p; do
  [ -z "$p" ] && continue
  PARAMS=$(echo "$HOST/molecule/$p" | sed -r 's/(.*)\,(.*)\,(.*)/-d \"\3 \2\" \1/')
  printf "$p - "
  eval "curl -X POST ""$PARAMS"
done <molecules.txt

printf "\nAdding catalysts:\n\n"

while read p; do
  [ -z "$p" ] && continue
  PARAMS=$(echo "$HOST/catalyst/$p" | sed -r 's/(.*)\,(.*)\,(.*)/-d \"\3 \2\" \1/')
  printf "$p - "
  eval "curl -X POST ""$PARAMS"
done <catalysts.txt

printf "\nAdding reactions:\n\n"

while read p; do
  [ -z "$p" ] && continue
  PARAMS=$(echo "$HOST/reaction/$p" | sed -r 's/(.*)\,(.*)/-d \"\2\" \1/')
  printf "$p - "
  eval "curl -X POST ""$PARAMS"
done <reactions.txt

printf "\nDone!\n"
