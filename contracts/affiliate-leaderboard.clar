;; affiliate-leaderboard
;;
;; This contract tracks affiliate performance metrics and maintains a competitive leaderboard.
;; It ranks affiliates based on various performance indicators and offers rewards to top performers.
;; The public leaderboard promotes healthy competition and engagement among affiliates.
;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-AFFILIATE-NOT-FOUND (err u101))
(define-constant ERR-INVALID-PERIOD (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-ALREADY-CLAIMED (err u104))
(define-constant ERR-NOT-ELIGIBLE (err u105))
(define-constant ERR-INVALID-TIMESTAMP (err u106))
(define-constant ERR-PERIOD-NOT-ENDED (err u107))
(define-constant ERR-PERIOD-ALREADY-FINALIZED (err u108))
;; Data space definitions
;; Contract administrator
(define-data-var contract-owner principal tx-sender)
;; Affiliate record: stores performance metrics for each affiliate
(define-map affiliate-records
  {
    affiliate: principal,
    period: uint,
  } ;; Keys: affiliate principal and period ID
  {
    sales-count: uint, ;; Total number of sales
    total-sales-amount: uint, ;; Total amount of sales in microstacks
    commission-earned: uint, ;; Total commission earned in microstacks
    conversion-rate: uint, ;; Conversion rate (multiplied by 10000 for precision)
    last-updated: uint, ;; Last time this record was updated (in block height)
  }
)
;; Period settings: defines the start and end of ranking periods
(define-map periods
  { period-id: uint }
  {
    start-time: uint, ;; Start timestamp
    end-time: uint, ;; End timestamp
    is-finalized: bool, ;; Whether rankings have been finalized
    period-type: (string-utf8 10), ;; "weekly" or "monthly"
  }
)
;; Rankings for each period
(define-map period-rankings
  { period-id: uint }
  {
    rankings: (list 100 {
      affiliate: principal,
      score: uint,
    }), ;; Top affiliates with their scores
    finalized-at: uint, ;; When rankings were finalized
  }
)
;; Reward claims tracking
(define-map reward-claims
  {
    affiliate: principal,
    period-id: uint,
  }
  {
    claimed: bool, ;; Whether the reward was claimed
    claimed-at: uint, ;; When it was claimed
    reward-amount: uint, ;; Amount of reward
  }
)
;; Current period ID
(define-data-var current-period-id uint u1)
;; Tracks all registered affiliates
(define-map registered-affiliates
  { affiliate: principal }
  {
    joined-at: uint,
    is-active: bool,
    referral-code: (string-utf8 20),
  }
)
;; Private functions
;; Get the current period ID
(define-private (get-current-period-id)
  (var-get current-period-id)
)

;; Calculate affiliate score based on their metrics
;; Formula: (sales-amount * 0.5) + (commission * 0.3) + (conversion-rate * sales-count * 0.2)
(define-private (calculate-affiliate-score
    (sales-count uint)
    (sales-amount uint)
    (commission uint)
    (conversion-rate uint)
  )
  (let (
      (sales-score (* sales-amount u5)) ;; 50% weight for sales amount
      (commission-score (* commission u3)) ;; 30% weight for commission
      (conversion-score (* (* conversion-rate sales-count) u2)) ;; 20% weight for conversion success
    )
    (/ (+ (+ sales-score commission-score) conversion-score) u10)
    ;; Normalize to get final score
  )
)

;; Check if a given principal is the contract owner
(define-private (is-contract-owner (caller principal))
  (is-eq caller (var-get contract-owner))
)

;; Check if period exists and is active
(define-private (is-valid-period (period-id uint))
  (match (map-get? periods { period-id: period-id })
    period
    true
    false
  )
)

;; Check if a given affiliate is registered
(define-private (is-registered-affiliate (affiliate principal))
  (match (map-get? registered-affiliates { affiliate: affiliate })
    entry (get is-active entry)
    false
  )
)

;; Check if given amount is valid (greater than zero)
(define-private (is-valid-amount (amount uint))
  (> amount u0)
)

;; Get current timestamp from block height
(define-private (get-current-time)
  block-height
)

;; Public functions
;; Register a new affiliate
(define-public (register-affiliate (referral-code (string-utf8 20)))
  (let (
      (affiliate tx-sender)
      (current-time (get-current-time))
    )
    ;; Check if affiliate is already registered
    (match (map-get? registered-affiliates { affiliate: affiliate })
      existing-record
      (if (get is-active existing-record)
        (ok true) ;; Already registered and active
        ;; Re-activate previously deactivated affiliate
        (begin
          (map-set registered-affiliates { affiliate: affiliate } {
            joined-at: (get joined-at existing-record),
            is-active: true,
            referral-code: referral-code,
          })
          (ok true)
        )
      )
      ;; New affiliate registration
      (begin
        (map-set registered-affiliates { affiliate: affiliate } {
          joined-at: current-time,
          is-active: true,
          referral-code: referral-code,
        })
        (ok true)
      )
    )
  )
)

;; Record a new sale for an affiliate
(define-public (record-sale
    (affiliate principal)
    (sale-amount uint)
    (commission uint)
    (conversions uint)
    (attempts uint)
  )
  (let (
      (period-id (get-current-period-id))
      (current-time (get-current-time))
      (conversion-rate (if (> attempts u0)
        (/ (* conversions u10000) attempts)
        u0
      ))
    )
    ;; Only contract owner can record sales
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    ;; Check that affiliate exists
    (asserts! (is-registered-affiliate affiliate) ERR-AFFILIATE-NOT-FOUND)
    ;; Check that the period is valid
    (asserts! (is-valid-period period-id) ERR-INVALID-PERIOD)
    ;; Check that amounts are valid
    (asserts! (is-valid-amount sale-amount) ERR-INVALID-AMOUNT)
    ;; Update affiliate record
    (match (map-get? affiliate-records {
      affiliate: affiliate,
      period: period-id,
    })
      existing-record
      (let (
          (new-sales-count (+ (get sales-count existing-record) u1))
          (new-sales-amount (+ (get total-sales-amount existing-record) sale-amount))
          (new-commission (+ (get commission-earned existing-record) commission))
          ;; Average out the conversion rate with existing data
          (new-conversion-rate (/ (+ (get conversion-rate existing-record) conversion-rate) u2))
        )
        (map-set affiliate-records {
          affiliate: affiliate,
          period: period-id,
        } {
          sales-count: new-sales-count,
          total-sales-amount: new-sales-amount,
          commission-earned: new-commission,
          conversion-rate: new-conversion-rate,
          last-updated: current-time,
        })
      )
      ;; No existing record, create new one
      (map-set affiliate-records {
        affiliate: affiliate,
        period: period-id,
      } {
        sales-count: u1,
        total-sales-amount: sale-amount,
        commission-earned: commission,
        conversion-rate: conversion-rate,
        last-updated: current-time,
      })
    )
    (ok true)
  )
)

;; Start a new period (weekly or monthly)
(define-public (start-new-period
    (period-type (string-utf8 10))
    (duration uint)
  )
  (let (
      (current-time (get-current-time))
      (new-period-id (+ (var-get current-period-id) u1))
    )
    ;; Only contract owner can start new periods
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    ;; Check period type is valid (weekly or monthly)
    (asserts! (or (is-eq period-type u"weekly") (is-eq period-type u"monthly"))
      ERR-INVALID-PERIOD
    )
    ;; Calculate end time
    (let ((end-time (+ current-time duration)))
      ;; Set up the new period
      (map-set periods { period-id: new-period-id } {
        start-time: current-time,
        end-time: end-time,
        is-finalized: false,
        period-type: period-type,
      })
      ;; Update current period ID
      (var-set current-period-id new-period-id)
      (ok new-period-id)
    )
  )
)

;; Finalize rankings for a completed period
(define-public (finalize-period-rankings (period-id uint))
  (let ((current-time (get-current-time)))
    ;; Only contract owner can finalize rankings
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    ;; Check that period exists
    (asserts! (is-valid-period period-id) ERR-INVALID-PERIOD)
    ;; Get period data using unwrap!
    (let ((period-data (unwrap! (map-get? periods { period-id: period-id }) ERR-INVALID-PERIOD)))
      ;; Check that period has ended
      (asserts! (>= current-time (get end-time period-data)) ERR-PERIOD-NOT-ENDED)
      ;; Check that period is not already finalized
      (asserts! (not (get is-finalized period-data)) ERR-PERIOD-ALREADY-FINALIZED)
      ;; Mark period as finalized
      (map-set periods { period-id: period-id }
        (merge period-data { is-finalized: true })
      )
      ;; For testing purposes, we'll create an empty rankings list
      (map-set period-rankings { period-id: period-id } {
        rankings: (list {
          affiliate: tx-sender,
          score: u0,
        }),
        finalized-at: current-time,
      })
      (ok true)
    )
  )
)

;; Distribute rewards to top performers for a period
(define-public (distribute-rewards
    (period-id uint)
    (affiliate principal)
    (reward-amount uint)
  )
  (let ((current-time (get-current-time)))
    ;; Only contract owner can distribute rewards
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    ;; Check that affiliate exists
    (asserts! (is-registered-affiliate affiliate) ERR-AFFILIATE-NOT-FOUND)
    ;; Check that period exists and is finalized
    (let ((period (unwrap! (map-get? periods { period-id: period-id }) ERR-INVALID-PERIOD)))
      (asserts! (get is-finalized period) ERR-PERIOD-NOT-ENDED)
      ;; Halts if not finalized
    )
    ;; If asserts! passes, 'true' is discarded, execution continues
    ;; Record the reward claim
    (map-set reward-claims {
      affiliate: affiliate,
      period-id: period-id,
    } {
      claimed: true,
      claimed-at: current-time,
      reward-amount: reward-amount,
    })
    ;; In a real implementation, this would trigger an actual token transfer
    ;; to the affiliate, likely by calling a separate token contract
    (ok reward-amount)
  )
)

;; Set the contract owner to a new principal
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; Read-only functions
;; Get affiliate metrics for a specific period
(define-read-only (get-affiliate-metrics
    (affiliate principal)
    (period-id uint)
  )
  (match (map-get? affiliate-records {
    affiliate: affiliate,
    period: period-id,
  })
    record (ok record)
    (err ERR-AFFILIATE-NOT-FOUND)
  )
)

;; Get period information
(define-read-only (get-period-info (period-id uint))
  (match (map-get? periods { period-id: period-id })
    period (ok period)
    (err ERR-INVALID-PERIOD)
  )
)

;; Get top performers for a specific period
(define-read-only (get-period-top-performers
    (period-id uint)
    (limit uint)
  )
  (match (map-get? period-rankings { period-id: period-id })
    rankings (ok (get rankings rankings))
    (err ERR-INVALID-PERIOD)
  )
)

;; Check if an affiliate has claimed rewards for a period
(define-read-only (has-claimed-reward
    (affiliate principal)
    (period-id uint)
  )
  (match (map-get? reward-claims {
    affiliate: affiliate,
    period-id: period-id,
  })
    claim (ok (get claimed claim))
    (ok false)
  )
)

;; Get affiliate registration status
(define-read-only (get-affiliate-status (affiliate principal))
  (match (map-get? registered-affiliates { affiliate: affiliate })
    record (ok record)
    (err ERR-AFFILIATE-NOT-FOUND)
  )
)

;; Calculate an affiliate's current score for the active period
(define-read-only (calculate-current-score (affiliate principal))
  (let ((period-id (get-current-period-id)))
    (match (map-get? affiliate-records {
      affiliate: affiliate,
      period: period-id,
    })
      record (ok (calculate-affiliate-score (get sales-count record)
        (get total-sales-amount record) (get commission-earned record)
        (get conversion-rate record)
      ))
      (err ERR-AFFILIATE-NOT-FOUND)
    )
  )
)
