(ns musicbrainz.test
  (:use [musicbrainz] :reload)
  (:use [clojure.test]))

(deftest test-search
  (is true (list? (search "The Decemberists"))))
