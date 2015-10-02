:; exec emacs -Q --script "$0" -- "$@" # -*- lexical-binding: t -*-

(setq tdd-lib-dir (concat (file-name-directory load-file-name) "/.."))
(push tdd-lib-dir load-path)
(push (file-name-directory load-file-name) load-path)

(require 'tdd-helper)
(require 'wordnut-history)

(ert-deftest history-clean()
  (let ((hs (make-wordnut--h)))
    (wordnut--h-clean hs)
    (should (= (wordnut--h-pos hs) -1))
    ))

(ert-deftest history-new()
  (let ((item (wordnut--h-item-new "foo")) )
    (should (equal "foo" (cdr (assoc 'name item))))
    ))

(ert-deftest history-add()
  (let ((hs (make-wordnut--h)))
    (wordnut--h-add hs (wordnut--h-item-new "1"))
    (wordnut--h-add hs (wordnut--h-item-new "2"))
    (wordnut--h-add hs (wordnut--h-item-new "3"))
    (wordnut--h-add hs (wordnut--h-item-new "2"))

    (should (equal '("2" "3" "1") (wordnut--h-names hs)))

    (setf (wordnut--h-pos hs) 1)
    (wordnut--h-add hs (wordnut--h-item-new "1"))
    (should (equal '("2" "1" "3") (wordnut--h-names hs)))

    (should (= 3 (length (wordnut--h-list hs))))
    ))

(ert-deftest history-size1()
  (let ((hs (make-wordnut--h)))

    (wordnut--h-add hs (wordnut--h-item-new "1"))
    (wordnut--h-add hs (wordnut--h-item-new "2"))
    (wordnut--h-add hs (wordnut--h-item-new "3"))
    (wordnut--h-add hs (wordnut--h-item-new "4"))
    (wordnut--h-add hs (wordnut--h-item-new "5"))
    (wordnut--h-add hs (wordnut--h-item-new "6"))
    (wordnut--h-add hs (wordnut--h-item-new "7"))
    (wordnut--h-add hs (wordnut--h-item-new "8"))
    (wordnut--h-add hs (wordnut--h-item-new "9"))
    (wordnut--h-add hs (wordnut--h-item-new "10"))

    (setf (wordnut--h-pos hs) 5)
    (setf (wordnut--h-max hs) 3)

    (wordnut--h-add hs (wordnut--h-item-new "1"))
    (should (= 7 (length (wordnut--h-list hs))))
    ))

(ert-deftest history-size2()
  (let ((hs (make-wordnut--h)))

    (setf (wordnut--h-max hs) 3)

    (wordnut--h-add hs (wordnut--h-item-new "1"))
    (wordnut--h-add hs (wordnut--h-item-new "2"))
    (wordnut--h-add hs (wordnut--h-item-new "3"))
    (wordnut--h-add hs (wordnut--h-item-new "4"))
    (wordnut--h-add hs (wordnut--h-item-new "5"))
    (wordnut--h-add hs (wordnut--h-item-new "6"))
    (wordnut--h-add hs (wordnut--h-item-new "7"))
    (wordnut--h-add hs (wordnut--h-item-new "8"))
    (wordnut--h-add hs (wordnut--h-item-new "9"))
    (wordnut--h-add hs (wordnut--h-item-new "10"))

    (setf (wordnut--h-pos hs) 2)

    (wordnut--h-add hs (wordnut--h-item-new "1"))
;    (pp (wordnut--h-list hs))
    (should (= 5 (length (wordnut--h-list hs))))
    ))

(ert-deftest history-find()
  (let ((hs (make-wordnut--h)) item)

    (wordnut--h-add hs (wordnut--h-item-new "1"))
    (wordnut--h-add hs (wordnut--h-item-new "2"))
    (wordnut--h-add hs (wordnut--h-item-new "3"))

    (should-not (wordnut--h-find hs "qqq"))

    (setq item (wordnut--h-find hs "2"))
    (should item)
    (should (equal "2" (cdr (assoc 'name item))))
    ))

(ert-deftest equal-words()
  (should (wordnut--h-equal-words? "part with" "part with"))
  (should (wordnut--h-equal-words? "part with" "part_with"))
  (should (wordnut--h-equal-words? "underscores_are_everywhere" "underscores are everywhere"))
  (should (wordnut--h-equal-words? "underscores_are_everywhere" "underscores are_everywhere"))
  (should (wordnut--h-equal-words? "part WITH" "PART_with"))
  (should-not (wordnut--h-equal-words? "part" "PART_with"))

  (should (wordnut--h-equal-words? "part_with" "part-with"))
  )

(ert-deftest slice()
  (should-not (wordnut--h-slice nil 0))
  (should-not (wordnut--h-slice nil 0 -1))
  (should-not (wordnut--h-slice nil -1 -1))
  (should-not (wordnut--h-slice nil 0 19))
  (should (equal '(1 2 3) (wordnut--h-slice '(1 2 3) -5)))
  (should (equal '(1 2 3) (wordnut--h-slice '(1 2 nil 3) -5)))
  (should (equal '(1 2 3) (wordnut--h-slice '(1 2 3) -5 -5)))
  )



(ert-run-tests-batch-and-exit (car argv))
