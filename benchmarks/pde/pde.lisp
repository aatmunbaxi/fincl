;;;; pde.lisp --- finite differences, at the same grid on both sides
;;;;
;;;; Grids are time steps x space nodes (x variance nodes for Heston). Both
;;;; sides use Crank-Nicolson in log-spot (Douglas, theta = 1/2, in QuantLib)
;;;; and Hundsdorfer-Verwer ADI for Heston; fincl adds Rannacher start-up
;;;; (two implicit half-steps after each payoff date), QuantLib has no damping
;;;; steps by default; their space grids are concentrated differently.
;;;;
;;;;   benchmark               QuantLib                                       fincl                  grid
;;;;   fd-european-bs          FdBlackScholesVanillaEngine                    :fd                    200 x 200
;;;;   fd-american-put-bs      FdBlackScholesVanillaEngine                    :fd                    400 x 400
;;;;   fd-bermudan-put-bs      FdBlackScholesVanillaEngine                    :fd                    400 x 400
;;;;   fd-barrier-dko          FdBlackScholesBarrierEngine                    :fd                    400 x 400
;;;;   fd-american-put-local   FdBlackScholesVanillaEngine, local vol         :fd, make-local-vol    400 x 400
;;;;   fd-heston-european      FdHestonVanillaEngine                          :fd, Heston            100 x 100 x 50
;;;;   fd-heston-american      FdHestonVanillaEngine                          :fd, Heston            100 x 100 x 50

(in-package #:fincl/benchmarks)

(defun fd (n-time n-space &rest settings)
  (apply #'make-engine :fd :n-time n-time :n-space n-space settings))

(defparameter *fd-notes*
  "QuantLib: Douglas (Crank-Nicolson) without damping steps; fincl: Crank-Nicolson
with Rannacher start-up; different space-grid concentrations")

(define-benchmark "fd-european-bs"
    (:group :pde :title "European call, Black-Scholes PDE" :engine "fd"
     :inputs (equity-inputs :kind "call") :work '(:time-steps 200 :space-nodes 200)
     :kind :method-error :tolerance 1d-3 :notes *fd-notes*
     :reference (price o :black-scholes m))
    ((m (equity-market)) (o (vanilla :call)) (e (fd 200 200)))
  (priced o e m))

(define-benchmark "fd-american-put-bs"
    (:group :pde :title "American put, Black-Scholes PDE" :engine "fd"
     :inputs (equity-inputs :kind "put" :exercise "american") :work '(:time-steps 400 :space-nodes 400)
     :kind :method-error :tolerance 1d-3
     :notes "fincl: Ikonen-Toivanen splitting; QuantLib: projection after each step"
     :reference (price o (make-engine :binomial :n-steps 10000) m))
    ((m (equity-market)) (o (vanilla :put :exercise :american)) (e (fd 400 400)))
  (priced o e m))

(define-benchmark "fd-bermudan-put-bs"
    (:group :pde :title "Bermudan put, quarterly, Black-Scholes PDE" :engine "fd"
     :inputs (equity-inputs :kind "put" :exercise "bermudan") :work '(:time-steps 400 :space-nodes 400)
     :kind :method-error :tolerance 1d-3 :notes *fd-notes*
     :reference (price o (fourier (make-gbm) 2048) m))
    ((m (equity-market)) (o (vanilla :put :exercise :bermudan)) (e (fd 400 400)))
  (priced o e m))

(define-benchmark "fd-barrier-dko"
    (:group :pde :title "Down-and-out call, continuous, Black-Scholes PDE" :engine "fd"
     :inputs (equity-inputs :kind "call" :barrier-type "down-and-out" :barrier 90 :rebate 0)
     :work '(:time-steps 400 :space-nodes 400)
     :kind :method-error :tolerance 1d-3
     :notes "both grids end at the barrier with the rebate as a Dirichlet boundary"
     :reference (price o :black-scholes m))
    ((m (equity-market)) (o (barrier-option :call :down-and-out 90)) (e (fd 400 400)))
  (priced o e m))

(define-benchmark "fd-american-put-local"
    (:group :pde :title "American put, Dupire local vol of an SSVI surface" :engine "fd (local vol)"
     :inputs (apply #'equity-inputs :kind "put" :exercise "american" :vol "ssvi" *ssvi*)
     :work '(:time-steps 400 :space-nodes 400)
     :kind :method-error :tolerance 5d-3
     :notes "the same SSVI surface on both sides (common.hpp's SsviVolatility);
QuantLib's LocalVolSurface takes Dupire's derivatives by finite differences at
every node and step, fincl analytically, once per step on its grid")
    ((m (equity-market :vol (ssvi-surface))) (o (vanilla :put :exercise :american))
     (e (fd 400 400 :process (make-local-vol))))
  (priced o e m))

(define-benchmark "fd-heston-european"
    (:group :pde :title "European call, Heston ADI" :engine "fd (Heston)"
     :inputs (heston-inputs :kind "call") :work '(:time-steps 100 :space-nodes 100 :variance-nodes 50)
     :kind :method-error :tolerance 2d-3
     :notes "both Hundsdorfer-Verwer; fincl damps the payoff with Douglas theta = 1
substeps, QuantLib not by default"
     :reference (price o (fourier p 1024) m))
    ((m (equity-market)) (o (vanilla :call)) (p (apply #'make-heston *heston*))
     (e (fd 100 100 :process p :n-variance 50)))
  (priced o e m))

(define-benchmark "fd-heston-american"
    (:group :pde :title "American put, Heston ADI" :engine "fd (Heston)"
     :inputs (heston-inputs :kind "put" :exercise "american")
     :work '(:time-steps 100 :space-nodes 100 :variance-nodes 50)
     :kind :method-error :tolerance 2d-3
     :notes "fincl: Ikonen-Toivanen splitting in the ADI steps; QuantLib: projection")
    ((m (equity-market)) (o (vanilla :put :exercise :american))
     (e (fd 100 100 :process (apply #'make-heston *heston*) :n-variance 50)))
  (priced o e m))
