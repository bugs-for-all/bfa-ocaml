  $ ../bins/batch.exe
  (declare-datatype Ptr ((mk-ptr (loc Int) (ofs Int))))
  (declare-datatype Opt (par (P) ((mk-some (opt-unwrap P)) (none))))
  (push 1)
  (declare-fun |0| () Int)
  (assert (<= 0 |0|))
  (assert (< |0| 10))
  (assert (< |0| 0))
  (check-sat)
