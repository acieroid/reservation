DIR="benchmark_results"

echo > results.txt

for file in $(ls $DIR/*.txt); do
  columns="$(echo $file | sed -Ee 's/[\/a-z_\.]+/ /g')"
  cat $file | sed -Ee "s/$/$columns/" | sed -Ee 's/ $//' >> results.txt
done
