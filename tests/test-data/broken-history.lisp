(:SEQUENCE 1 :CLASS LIST :SIZE 2 :ELEMENTS
 ("2.2.4"
  (:OBJECT 2 :CLASS HISTORY-TREE:HISTORY-TREE :SLOTS
   ((HISTORY-TREE:OWNERS :HASH-TABLE 3 :TEST EQUALP :SIZE 7 :REHASH-SIZE 1.5
                         :REHASH-THRESHOLD 1.0 :ENTRIES
                         (("2" :OBJECT 4 :CLASS HISTORY-TREE:OWNER :SLOTS
                           ((HISTORY-TREE:ORIGIN :OBJECT 5 :CLASS
                                                 HISTORY-TREE:NODE :SLOTS
                                                 ((HISTORY-TREE:PARENT)
                                                  (HISTORY-TREE:XXX) ; XXX: Invalid slot inserted for testing purposes.
                                                  (HISTORY-TREE:CHILDREN)
                                                  (HISTORY-TREE:BINDINGS
                                                   :HASH-TABLE 6 :TEST EQL
                                                   :SIZE 7 :REHASH-SIZE 1.5
                                                   :REHASH-THRESHOLD 1.0
                                                   :ENTRIES
                                                   (((:REF . 4) :OBJECT 7
                                                     :CLASS
                                                     HISTORY-TREE::BINDING
                                                     :SLOTS
                                                     ((HISTORY-TREE::FORWARD-CHILD)
                                                      (HISTORY-TREE:LAST-ACCESS
                                                       . "2022-09-13T13:50:36.812824Z")))))
                                                  (HISTORY-TREE:ENTRY :OBJECT 8
                                                                      :CLASS
                                                                      HISTORY-TREE:ENTRY
                                                                      :SLOTS
                                                                      ((HISTORY-TREE::HISTORY
                                                                        :REF
                                                                        . 2)
                                                                       (HISTORY-TREE:DATA
                                                                        :OBJECT
                                                                        9
                                                                        :CLASS
                                                                        HISTORY-ENTRY
                                                                        :SLOTS
                                                                        ((URL
                                                                          . "https://duckduckgo.com/?q=foo&ia=web")
                                                                         (TITLE
                                                                          . "foo at DuckDuckGo")
                                                                         (LAST-ACCESS
                                                                          . "")
                                                                         (EXPLICIT-VISITS
                                                                          . 0)
                                                                         (IMPLICIT-VISITS
                                                                          . 1)
                                                                         (SCROLL-POSITION)))
                                                                       (HISTORY-TREE:LAST-ACCESS
                                                                        . "2022-09-13T13:50:36.797152Z")
                                                                       (HISTORY-TREE:NODES
                                                                        :SEQUENCE
                                                                        10
                                                                        :CLASS
                                                                        LIST
                                                                        :SIZE 1
                                                                        :ELEMENTS
                                                                        ((:REF
                                                                          . 5)))))))
                            (HISTORY-TREE:CREATOR-ID)
                            (HISTORY-TREE::CREATOR-NODE)
                            (HISTORY-TREE:CURRENT :REF . 5)
                            (HISTORY-TREE:NODES :SEQUENCE 11 :CLASS LIST :SIZE
                                                1 :ELEMENTS ((:REF . 5)))))
                          ("3" :OBJECT 12 :CLASS HISTORY-TREE:OWNER :SLOTS
                           ((HISTORY-TREE:ORIGIN :OBJECT 13 :CLASS
                                                 HISTORY-TREE:NODE :SLOTS
                                                 ((HISTORY-TREE:PARENT)
                                                  (HISTORY-TREE:CHILDREN
                                                   :SEQUENCE 14 :CLASS LIST
                                                   :SIZE 1 :ELEMENTS
                                                   ((:OBJECT 15 :CLASS
                                                     HISTORY-TREE:NODE :SLOTS
                                                     ((HISTORY-TREE:PARENT :REF
                                                                           . 13)
                                                      (HISTORY-TREE:CHILDREN)
                                                      (HISTORY-TREE:BINDINGS
                                                       :HASH-TABLE 16 :TEST EQL
                                                       :SIZE 7 :REHASH-SIZE 1.5
                                                       :REHASH-THRESHOLD 1.0
                                                       :ENTRIES
                                                       (((:REF . 12) :OBJECT 17
                                                         :CLASS
                                                         HISTORY-TREE::BINDING
                                                         :SLOTS
                                                         ((HISTORY-TREE::FORWARD-CHILD)
                                                          (HISTORY-TREE:LAST-ACCESS
                                                           . "2022-09-13T13:50:42.932796Z")))))
                                                      (HISTORY-TREE:ENTRY
                                                       :OBJECT 18 :CLASS
                                                       HISTORY-TREE:ENTRY
                                                       :SLOTS
                                                       ((HISTORY-TREE::HISTORY
                                                         :REF . 2)
                                                        (HISTORY-TREE:DATA
                                                         :OBJECT 19 :CLASS
                                                         HISTORY-ENTRY :SLOTS
                                                         ((URL
                                                           . "https://duckduckgo.com/?q=bar&ia=places")
                                                          (TITLE
                                                           . "bar at DuckDuckGo")
                                                          (LAST-ACCESS . "")
                                                          (EXPLICIT-VISITS . 0)
                                                          (IMPLICIT-VISITS . 1)
                                                          (SCROLL-POSITION)))
                                                        (HISTORY-TREE:LAST-ACCESS
                                                         . "2022-09-13T13:50:42.932707Z")
                                                        (HISTORY-TREE:NODES
                                                         :SEQUENCE 20 :CLASS
                                                         LIST :SIZE 1 :ELEMENTS
                                                         ((:REF . 15)))))))))
                                                  (HISTORY-TREE:BINDINGS
                                                   :HASH-TABLE 21 :TEST EQL
                                                   :SIZE 7 :REHASH-SIZE 1.5
                                                   :REHASH-THRESHOLD 1.0
                                                   :ENTRIES
                                                   (((:REF . 12) :OBJECT 22
                                                     :CLASS
                                                     HISTORY-TREE::BINDING
                                                     :SLOTS
                                                     ((HISTORY-TREE::FORWARD-CHILD
                                                       :REF . 15)
                                                      (HISTORY-TREE:LAST-ACCESS
                                                       . "2022-09-13T13:50:42.871392Z")))))
                                                  (HISTORY-TREE:ENTRY :OBJECT
                                                                      23 :CLASS
                                                                      HISTORY-TREE:ENTRY
                                                                      :SLOTS
                                                                      ((HISTORY-TREE::HISTORY
                                                                        :REF
                                                                        . 2)
                                                                       (HISTORY-TREE:DATA
                                                                        :OBJECT
                                                                        24
                                                                        :CLASS
                                                                        HISTORY-ENTRY
                                                                        :SLOTS
                                                                        ((URL
                                                                          . "https://duckduckgo.com/?q=bar")
                                                                         (TITLE
                                                                          . "bar at DuckDuckGo")
                                                                         (LAST-ACCESS
                                                                          . "")
                                                                         (EXPLICIT-VISITS
                                                                          . 0)
                                                                         (IMPLICIT-VISITS
                                                                          . 1)
                                                                         (SCROLL-POSITION)))
                                                                       (HISTORY-TREE:LAST-ACCESS
                                                                        . "2022-09-13T13:50:42.871323Z")
                                                                       (HISTORY-TREE:NODES
                                                                        :SEQUENCE
                                                                        25
                                                                        :CLASS
                                                                        LIST
                                                                        :SIZE 1
                                                                        :ELEMENTS
                                                                        ((:REF
                                                                          . 13)))))))
                            (HISTORY-TREE:CREATOR-ID)
                            (HISTORY-TREE::CREATOR-NODE)
                            (HISTORY-TREE:CURRENT :REF . 15)
                            (HISTORY-TREE:NODES :SEQUENCE 26 :CLASS LIST :SIZE
                                                2 :ELEMENTS
                                                ((:REF . 15) (:REF . 13)))))))
    (HISTORY-TREE:ENTRIES :HASH-TABLE 27 :TEST HISTORY-TREE::ENTRY-EQUAL-P
                          :SIZE 7 :REHASH-SIZE 1.5 :REHASH-THRESHOLD 1.0
                          :ENTRIES
                          (((:REF . 8) :REF . 8) ((:REF . 23) :REF . 23)
                           ((:REF . 18) :REF . 18)))
    (HISTORY-TREE:KEY . HISTORY-TREE-KEY) (HISTORY-TREE:TEST . EQUALP)
    (HISTORY-TREE:HASH-FUNCTION . SXHASH)))))
