(require 'ert)
(require 'icloud)

(ert-deftest test-icloud:symbol-to-keyword ()
  (let ((s 'foo)
        (k ':foo))
    (should (equal (icloud:symbol-to-keyword s)
                   k))))

(ert-deftest test-icloud:convert-alist-keys-to-chain ()
  (let ((alist '((fooBar . "aaa") (hogeHuga . "bbb")))
        (plist '((:foo-bar . "aaa") (:hoge-huga . "bbb"))))
    (should (equal (icloud:convert-alist-keys-to-chain alist)
                   plist))))

(ert-deftest test-icloud:alist-to-plist ()
  (let ((alist '((:foo . "aaa") (:bar . "bbb")))
        (plist '(:foo "aaa" :bar "bbb")))
    (should (equal (icloud:alist-to-plist alist)
                   plist))))

(ert-deftest test-icloud:to-chain ()
  (let ((camel "fooBarHoge")
        (chain "foo-bar-hoge"))
    (should (equal (icloud:to-chain camel)
                   chain))))

(ert-deftest test-icloud:generate-query ()
  (let ((plist '(:foo "aaa" :bar "bbb"))
        (query "foo=aaa&bar=bbb"))
    (should (equal (icloud:generate-query plist)
                   query))))

(ert-run-tests-batch-and-exit)
