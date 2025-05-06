;; Affiliate Marketing Contract
;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-not-found (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-campaign-expired (err u104))
(define-constant err-invalid-status (err u105))
;; Status levels
(define-constant status-bronze u1)
(define-constant status-silver u2)
(define-constant status-gold u3)
;; Data vars
(define-data-var commission-rate uint u10) ;; 10% default commission
(define-data-var silver-threshold uint u10) ;; 10 sales to reach silver
(define-data-var gold-threshold uint u25) ;; 25 sales to reach gold
(define-data-var commission-holding-period uint u0) ;; No holding period by default
;; Data maps
(define-map affiliates
  principal
  {
    earned: uint,
    referrals: uint,
    active: bool,
    status: uint,
    pending-commissions: (list 100 {
      amount: uint,
      unlock-height: uint,
    }),
  }
)
(define-map products
  uint
  {
    price: uint,
    owner: principal,
    active: bool,
    category: (optional uint),
  }
)
(define-map tier-commission-rates
  uint
  uint
)
(define-map promotional-campaigns
  uint
  {
    product-id: uint,
    commission-rate: uint,
    start-height: uint,
    end-height: uint,
    active: bool,
  }
)
;; Public functions
(define-public (register-affiliate)
  (let ((affiliate-data (map-get? affiliates tx-sender)))
    (if (is-some affiliate-data)
      err-already-registered
      (begin
        (map-set affiliates tx-sender {
          earned: u0,
          referrals: u0,
          active: true,
          status: status-bronze,
          pending-commissions: (list),
        })
        (ok true)
      )
    )
  )
)

(define-public (add-product
    (product-id uint)
    (price uint)
    (category (optional uint))
  )
  (begin
    (map-set products product-id {
      price: price,
      owner: tx-sender,
      active: true,
      category: category,
    })
    (ok true)
  )
)

;; Admin functions
(define-public (set-commission-rate (new-rate uint))
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set commission-rate new-rate)
      (ok true)
    )
    err-owner-only
  )
)

(define-public (set-tier-commission-rate
    (status uint)
    (rate uint)
  )
  (if (is-eq tx-sender contract-owner)
    (begin
      (map-set tier-commission-rates status rate)
      (ok true)
    )
    err-owner-only
  )
)

(define-public (set-status-thresholds
    (silver uint)
    (gold uint)
  )
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set silver-threshold silver)
      (var-set gold-threshold gold)
      (ok true)
    )
    err-owner-only
  )
)

(define-public (set-commission-holding-period (blocks uint))
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set commission-holding-period blocks)
      (ok true)
    )
    err-owner-only
  )
)

(define-public (create-promotional-campaign
    (campaign-id uint)
    (product-id uint)
    (rate uint)
    (duration uint)
  )
  (if (is-eq tx-sender contract-owner)
    (let (
        (product (unwrap! (map-get? products product-id) err-not-found))
        (current-block-height block-height)
        (end-height (+ current-block-height duration))
      )
      (begin
        (map-set promotional-campaigns campaign-id {
          product-id: product-id,
          commission-rate: rate,
          start-height: current-block-height,
          end-height: end-height,
          active: true,
        })
        (ok true)
      )
    )
    err-owner-only
  )
)

(define-public (end-promotional-campaign (campaign-id uint))
  (if (is-eq tx-sender contract-owner)
    (let ((campaign (unwrap! (map-get? promotional-campaigns campaign-id) err-not-found)))
      (begin
        (map-set promotional-campaigns campaign-id
          (merge campaign { active: false })
        )
        (ok true)
      )
    )
    err-owner-only
  )
)

;; Read only functions
(define-read-only (get-affiliate-stats (affiliate principal))
  (ok (map-get? affiliates affiliate))
)

(define-read-only (get-product (product-id uint))
  (ok (map-get? products product-id))
)

(define-read-only (get-commission-rate)
  (ok (var-get commission-rate))
)

(define-read-only (get-tier-commission-rate (status uint))
  (ok (map-get? tier-commission-rates status))
)

(define-read-only (get-promotional-campaign (campaign-id uint))
  (ok (map-get? promotional-campaigns campaign-id))
)

;; Helper functions
(define-private (determine-status (referrals uint))
  (if (>= referrals (var-get gold-threshold))
    status-gold
    (if (>= referrals (var-get silver-threshold))
      status-silver
      status-bronze
    )
  )
)

(define-private (get-campaign-rate
    (product-id uint)
    (current-height uint)
  )
  ;; Placeholder: Original logic with fold and map-keys is not directly implementable
  ;; as map-keys doesn't exist. Returning none for now.
  none
)

(define-private (release-ready (commission {
  amount: uint,
  unlock-height: uint,
}))
  (>= block-height (get unlock-height commission))
)

(define-private (not-release-ready (commission {
  amount: uint,
  unlock-height: uint,
}))
  (< block-height (get unlock-height commission))
)

(define-private (add-commission-amounts
    (amount uint)
    (commission {
      amount: uint,
      unlock-height: uint,
    })
  )
  (+ amount (get amount commission))
)
