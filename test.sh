#!/bin/sh
test() {
	cargo run testcases/$1.crb > testcases/$1.result.actual
	diff testcases/$1.result testcases/$1.result.actual 
	if [ $? -ne 0 ]; then { echo "\n\033[31mFAIL\033[m $1"; exit 1; }; else { echo "\n\033[32mPASS\033[m $1"; rm testcases/$1.result.actual; }; fi
}

# tests file IO as well as stdout
test2() {
	cargo run testcases/$1.crb > testcases/$1.result.actual
	diff testcases/$1.result testcases/$1.result.actual
	if [ $? -ne 0 ]; then { echo "\n\033[31mFAIL\033[m $1 - stdout"; exit 1; }; else { echo "\n\033[32mPASS\033[m $1 - stdout"; rm testcases/$1.result.actual; }; fi
	diff testcases/$1.fileio testcases/$1.fileio.actual
	if [ $? -ne 0 ]; then { echo "\n\033[31mFAIL\033[m $1 - fileio"; exit 1; }; else { echo "\n\033[32mPASS\033[m $1 - fileio"; rm testcases/$1.fileio.actual; }; fi
}

test fizzbuzz
test global_versus_local
test2 ftest
test test
test arr_without_method
test arr_ref
test arr0
test test_02_