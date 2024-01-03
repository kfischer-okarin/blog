(print "Building site...")
(with-temp-buffer
  (insert (concat "Hello, world at " (format-time-string "%Y-%m-%d %H:%M:%S") "."))
  (write-file "../dist/index.html"))
