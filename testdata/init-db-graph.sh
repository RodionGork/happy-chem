HOST=http://localhost:18080

while read p; do
  [ -z "$p" ] && continue
  PARAMS=$(echo "$HOST/$p" | sed -r 's/(\S+)\s(.*)/-d \"\2\" \1/')
  printf " $PARAMS - "
  eval "curl -X POST ""$PARAMS"
done <graph.txt
