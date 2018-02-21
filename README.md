# cl-muth

Various multithreading and concurrency utilities:

* `thread-pool` - conventional thread pool
* `guarded-reference` - a value holder with guarded by lock access to it
* `latch` (countdown latch) - synchronization primitive for blocking further execution until 0
  count is reached after a final countdown.
* `blocking-queue` (with 5 level of priorities) - synchronization primitive for blocking reads
  and writes from/into a queue
