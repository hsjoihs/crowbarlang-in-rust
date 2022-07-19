
clean:
	rm testcases/*.actual

test:
	cargo test
	./test.sh