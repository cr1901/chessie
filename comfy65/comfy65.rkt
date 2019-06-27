#lang racket

; No PLists in Racket
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))


;;; Define 6502 op codes.
;;; Basic test instructions.
(put 'c=1\? 'test 176) ;;; test carry=1.
(put 'c=0\? 'test 144) ;;; test carry=0.
(put 'llt 'test 144) ;;; logically <.
(put 'lge 'test 176) ;;; logically >=.
(put '=\? 'test 240) ;;; equal.
(put '~=\? 'test 208) ;;; not equal.
(put '=0\? 'test 240) ;;; equals zero.
(put '˜~=0\? 'test 208) ;;; not equal to zero.
(put 'v=1\? 'test 112) ;;; test overflow=1.
(put 'v=0\? 'test 80) ;;; test overflow=0.
(put '<\? 'test 48) ;;; test arithmetic <.
(put '>=\? 'test 16) ;;; test arithmetic >=.
(put '<0\? 'test 48) ;;; test arithmetic <0.
(put '>=0\? 'test 16) ;;; test arithmetic >=0.
;;; Group 0.
(put '\? 'skeleton 32) ;;; test.
(put 'stj 'skeleton 152) ;;; store j.
(put 'lj 'skeleton 168) ;;; load j.
(put 'cj 'skeleton 200) ;;; compare j.
(put 'ci 'skeleton 232) ;;; compare i.
;;; Group 1.
(put 'lor 'skeleton 17) ;;; logical or.
(put 'land 'skeleton 49) ;;; logical and.
(put 'lxor 'skeleton 81) ;;; logical xor.
(put '+ 'skeleton 113) ;;; add with carry.
(put 'st 'skeleton 145) ;;; store accumulator.
(put 'l 'skeleton 177) ;;; load accumulator.
(put 'c 'skeleton 209) ;;; compare accumulator.
(put '- 'skeleton 241) ;;; subtract with borrow.
;;; Group 2.
(put 'asl 'skeleton 10) ;;; arithmetic shift left.
(put 'rl 'skeleton 42) ;;; rotate left.
(put 'lsr 'skeleton 74) ;;; logical shift right.
(put 'rr 'skeleton 106) ;;; rotate right.
(put 'sti 'skeleton 138) ;;; store i.
(put 'li 'skeleton 170) ;;; load i.
(put '1- 'skeleton 194) ;;; decrement.
(put '1+ 'skeleton 226) ;;; increment.
;;; random instructions.
(put 'trap 'skeleton 0) ;;; programmed break.
(put 'save 'skeleton 8) ;;; push processor state onto stack.
(put 'restore 'skeleton 40) ;;; restore processor state from stack.
(put 'push 'skeleton 72) ;;; push accumulator onto stack.
(put 'pop 'skeleton 104) ;;; pop accumulator from stack.
(put 'c=0 'skeleton 24) ;;; clear carry.
(put 'c=1 'skeleton 56) ;;; set carry.
(put 'seb 'skeleton 24) ;;; set borrow.
(put 'clb 'skeleton 56) ;;; clear borrow.
(put 'v=0 'skeleton 184) ;;; clear overflow.
(put 'enable 'skeleton 88) ;;; enable interrupts.
(put 'disable 'skeleton 120) ;;; disable interrupts.
(put 'binary 'skeleton 216) ;;; set binary mode.
(put 'decimal 'skeleton 248) ;;; set decimal mode.
(put 'i+1 'skeleton 232) ;;; increment i.
(put 'j+1 'skeleton 200) ;;; increment j.
(put 'i-1 'skeleton 202) ;;; decrement i.
(put 'j-1 'skeleton 136) ;;; decrement j.
(put 'nop 'skeleton 234) ;;; no operation.

(put 'return 'jump 96)
(put 'resume 'jump 64)
(define jmp 76)
(define jsr 72)

(define mem (vector 10))