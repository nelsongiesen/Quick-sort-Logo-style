; How to use the sorter.
; We start with a Scheme dialect REPL.
; A REPL is the same type of instant interpretter that Python uses. But we need the one for Scheme dialect.
; Any of them will work, but I use the Dr.Racket REPL from the PLT team at Northeastern University and Brown University.
; We copy these definitions into the definitions window and hit run.
; We are ready to run the sorter in the interactions window.
; But the sorter wants a list: so we activate it like this (quick-sort (list 3 2 1 6 5 4 9 8 7)).
; The inside parentheses create a list and the outside parentheses run the sorter on that list.
; You can have much fun testing and tinkering with it.




                  ; All-same? is a question that takes a list. If all the items in the list are the same, it returns yes.
                  ; If any of the items is different, it returns no.
                  ; We compare each item one at a time.
                  ; If it is different, we return no. And we are done. EXIT.
                  ; If it is the same, we compare the next item.
                  ; If we run out of items, we know we are done. And we return yes. EXIT.                  
(define all-same? ; We use define to create a program, in this case we name it all-same?
  (lambda (input-ls) ; We use lambda to name our inputs: in this case we have one named input-ls.
    (if ; We use if to set up a fork in the road.
     (empty? input-ls) ; We use empty? to ask if input-ls is empty.
        yes ; If the answer is yes, we return yes. And we are done. EXIT.
        (let ; If the answer is no, we use let to create local variables, in this case we have one named matcher.
            ((matcher (first input-ls))) ; We assign matcher to the first item of input-ls.
          (letrec ((helper ; We use letrec to create helper programs, in this case we have one named helper.
                    (lambda (ls) ; We use lambda to name helper's inputs, in this case we have one named ls.
                      (if ; We use if to set up another fork in the road.
                       (empty? ls) ; We use empty? to ask if ls is empty.
                          yes ; If the answer is yes, we return yes. And we are done. EXIT.
                          (if ; If the answer is no, we use another if to set up another fork in the road.
                           (same? (first ls) matcher) ; We use same? to ask if the first item of ls is the same as matcher.
                              (helper ; If the answer is yes, we run helper with different inputs.
                                (without-first ls)) ; We change ls by removing its first item.
                              no))))) ; If the answer is no, we return no. And we are done. EXIT.
            (helper (without-first input-ls))))))) ; This last line is the initialization line.
                                                  ; We start helper with its original inputs.
                                                  ; We start ls as input-ls without its first item.

                ; Process is a program that takes a list.
                ; If all the items of the list are the same, it returns the list unchanged. And we are done. EXIT.
                ; If any of the items is different, it runs quick-sort on the list.
(define process ; We use define to create a program, in this case we name it process.
  (lambda (input-ls) ; We use lambda to name our inputs, in this case we have one named input-ls.
    (if ; We use if to set up a fork in the road.
     (all-same? input-ls) ; We use all-same? to ask if all the items of input-ls are the same.
        input-ls ; If the answer is yes, we return input-ls. And we are done. EXIT.
        (quick-sort input-ls)))) ; If the answer is no, we run quick-sort on input-ls.


                   ; Quick-sort is a program that takes an unsorted list and returns a sorted list.
                   ; It sorts the list into two parts: a lower part and an upper part.
                   ; And then it sorts the lower part into two and the higher part into two.
                   ; And it keeps sorting into two, until each list only has one number in it.
(define quick-sort ; We use define to create a program, in this case we name it quick-sort.
  (lambda (input-ls) ; We use lambda to name our inputs, in this case we have one named input-ls.
    (if ; We use if to set up a fork in the road.
     (empty? input-ls) ; We use empty? to ask if input-ls is empty.
        input-ls ; If the answer is yes, we return input-ls. And we are done. EXIT.
        (let ; If the answer is no, we use let to create local variables: in this case we have one named pivot.
            ((pivot (first input-ls))) ; We assign pivot to the first item of input-ls.
          (letrec ((helper ; We use letrec to create helper programs, in this case we have one named helper.
                    (lambda (ls lower-part higher-part) ; We use lambda to name helper's inputs: ls, lower-part, and higher-part.
                             (if ; We use if to set up another fork in the road.
                              (empty? ls) ; We use empty? to ask if ls is empty.
                                (combine ; If the answer is yes, we use combine to combine two lists into one list. And we are done. EXIT.
                                  (process lower-part) ; Our first list is the process of lower-part.
                                  (add-to-front pivot ; Our second is the process of higher-part with pivot added to front.
                                        (process higher-part))) 
                                (let ; If the answer is no, we use let to create local variables, in this case we have one named firsty.
                                     ((firsty (first ls))) ; We assign firsty to the first item of ls.
                                   (if ; We use if to set up another fork in the road.
                                    (less-than? firsty pivot) ; We use less-than? to ask if firsty is less than pivot.
                                       (helper ; If the answer is yes, we run helper with different inputs.
                                          (without-first ls) ; We change ls by removing its first item.
                                          (add-to-end lower-part firsty) ; We change lower-part by adding firsty to the end of it.
                                          higher-part) ; We leave higher-part unchanged.
                                       (helper ; If the answer is no, we run helper with different inputs.
                                          (without-first ls) ; We change ls by removing its first item.
                                          lower-part ; We leave lower-part unchanged.
                                          (add-to-end higher-part firsty)))))))) ; We change higher-part by adding firsty to the end of it.
            (helper (without-first input-ls) empty-list empty-list)))))) ; This last line is our initialization line.
                            ; We start helper with its original inputs.
                            ; We start ls as input-ls.
                            ; We start lower-part as an empty-list.
                            ; We start higher-part as an empty-list.
                                                                        


; Scheme dialect already has almost all of the programs that we need: we just changed them to Logo-style names.

(define first car)
(define without-first cdr)
(define same? equal?) 
(define empty? null?)
(define less-than? <)
(define empty-list '())
(define yes #t)
(define no #f)
(define add-to-front cons)
(define combine append)
(define listify list)

; Scheme dialect doesn't have a constructor that adds an item to the end of a list.
; But we can build one from two of our other constructors.
; We can already combine two lists together.
; And we can already make an item into a list.
; And that's all the magic we need to add an item to the end of a list.
(define add-to-end ; We use define to create a program: in this case we name it add-to-end.
  (lambda (ls item) ; We use lambda to name our inputs: in this case we have two named ls and item.
    (combine ; We use combine to combine two lists into one list.
       ls ; The first list is ls.
       (listify item)))) ; And the second list is the list containing item.



; Common alternate spellings

(define first-item first)
(define frist first)
(define without-first-item without-first)
(define without-frist-item without-first)
(define remove-first without-first)
(define remove-frist without-first)
(define remove-first-item without-first)
(define remove-frist-item without-first)
(define same same?)
(define smae same?)
(define sema same?)
(define smae? same?)
(define sema? same?)
(define empty empty?)
(define emtpy? empty?)
(define emtpy empty?)
(define less-than less-than?)
(define emptylist empty-list)
(define emtpy-list empty-list)
(define add-to-front-of-list add-to-front)
(define combine-lists combine)
(define combin-lists combine)
(define combin combine)
(define listfy listify)
(define listiy listify)
(define lstify listify)
(define add-to-end-of-list add-to-end)









