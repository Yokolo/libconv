for nbtests in 1 2 3 4 5;
do
	for nthreads in 2 4 6 8 16;
	do
		export OMP_NUM_THREADS=$nthreads
		./test_for >> RES/result_test_for_$nthreads.txt
		./test_tasks >> RES/result_test_tasks_$nthreads.txt
	done
done
