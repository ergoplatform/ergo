# AVL-IODB

To run benchmarks use command `sbt "project benchmarks" "jmh:run -i 3 -wi 3 -f1 -t1 -rf csv .*AVLTreeBatchPerformance"`

Results will be stored in `benchmarks/jmh-results.csv` file.