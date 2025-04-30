```clarity
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
(define-map affiliates principal
    {
        earned: uint,
        referrals: uint,
        active: bool,
        status: uint,
        pending-commissions: (list 100 {amount: uint, unlock-height: uint})
    }
)

(define-map products uint
    {
        price: uint,
        owner: principal,
        active: bool,
        category: (optional uint)
    }
)

(define-map tier-commission-rates uint uint)

(define-map promotional-campaigns uint
    {
        product-id: uint,
        commission-rate: uint,
        start-height: uint,
        end-height: uint,
        active: bool
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
                    pending-commissions: (list)
                })
                (ok true)
            )
        )
    )
)

(define-public (add-product (product-id uint) (price uint) (category (optional uint)))
    (begin
        (map-set products product-id {
            price: price,
            owner: tx-sender,
            active: true,
            category: category
        })
        (ok true)
    )
)

(define-public (process-sale (product-id uint) (affiliate principal))
    (let (
        (product (unwrap! (map-get? products product-id) err-not-found))
        (affiliate-data (unwrap! (map-get? affiliates affiliate) err-not-found))
        (current-block-height block-height)
        (campaign-rate (get-campaign-rate product-id current-block-height))
        (tier-rate (default-to (var-get commission-rate) (map-get? tier-commission-rates (get status affiliate-data))))
        (effective-rate (if (is-some campaign-rate) (unwrap-panic campaign-rate) tier-rate))
        (commission (/ (* (get price product) effective-rate) u100))
        (holding-period (var-get commission-holding-period))
        (unlock-height (+ current-block-height holding-period))
        (pending-commissions (get pending-commissions affiliate-data))
        (updated-commissions (if (> holding-period u0)
                                (append pending-commissions (list {amount: commission, unlock-height: unlock-height}))
                                pending-commissions))
        (new-referrals (+ (get referrals affiliate-data) u1))
        (new-status (determine-status new-referrals))
    )
    (if (>= (stx-get-balance tx-sender) (get price product))
        (begin
            ;; Transfer payment to product owner
            (try! (stx-transfer? (- (get price product) commission) tx-sender (get owner product)))
            
            ;; Handle commission based on holding period
            (if (> holding-period u0)
                ;; Store commission for later release
                (map-set affiliates affiliate {
                    earned: (get earned affiliate-data),
                    referrals: new-referrals,
                    active: true,
                    status: new-status,
                    pending-commissions: updated-commissions
                })
                ;; Immediate transfer
                (begin
                    (try! (stx-transfer? commission tx-sender affiliate))
                    (map-set affiliates affiliate {
                        earned: (+ (get earned affiliate-data) commission),
                        referrals: new-referrals,
                        active: true,
                        status: new-status,
                        pending-commissions: pending-commissions
                    })
                )
            )
            (ok true)
        )
        err-insufficient-funds
    ))
)

(define-public (claim-pending-commissions)
    (let (
        (affiliate-data (unwrap! (map-get? affiliates tx-sender) err-not-found))
        (current-block-height block-height)
        (pending-list (get pending-commissions affiliate-data))
        (claimable-commissions (filter release-ready pending-list))
        (remaining-commissions (filter not-release-ready pending-list))
        (total-claimable (fold add-commission-amounts u0 claimable-commissions))
    )
    (if (> (len claimable-commissions) u0)
        (begin
            (map-set affiliates tx-sender {
                earned: (+ (get earned affiliate-data) total-claimable),
                referrals: (get referrals affiliate-data),
                active: (get active affiliate-data),
                status: (get status affiliate-data),
                pending-commissions: remaining-commissions
            })
            (ok total-claimable)
        )
        (ok u0)
    ))
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

(define-public (set-tier-commission-rate (status uint) (rate uint))
    (if (is-eq tx-sender contract-owner)
        (begin
            (map-set tier-commission-rates status rate)
            (ok true)
        )
        err-owner-only
    )
)

(define-public (set-status-thresholds (silver uint) (gold uint))
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

(define-public (create-promotional-campaign (campaign-id uint) (product-id uint) (rate uint) (duration uint))
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
                active: true
            })
            (ok true)
        ))
        err-owner-only
    )
)

(define-public (end-promotional-campaign (campaign-id uint))
    (if (is-eq tx-sender contract-owner)
        (let ((campaign (unwrap! (map-get? promotional-campaigns campaign-id) err-not-found)))
            (begin
                (map-set promotional-campaigns campaign-id 
                    (merge campaign {active: false})
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

(define-private (get-campaign-rate (product-id uint) (current-height uint))
    (fold check-campaign none (map-keys promotional-campaigns))
    
    (define-private (check-campaign (campaign-id uint) (current-result (optional uint)))
        (if (is-some current-result)
            current-result
            (let ((campaign (unwrap! (map-get? promotional-campaigns campaign-id) none)))
                (if (and (is-eq (get product-id campaign) product-id)
                         (get active campaign)
                         (>= current-height (get start-height campaign))
                         (<= current-height (get end-height campaign)))
                    (some (get commission-rate campaign))
                    none
                )
            )
        )
    )
)

(define-private (release-ready (commission {amount: uint, unlock-height: uint}))
    (>= block-height (get unlock-height commission))
)

(define-private (not-release-ready (commission {amount: uint, unlock-height: uint}))
    (< block-height (get unlock-height commission))
)

(define-private (add-commission-amounts (amount uint) (commission {amount: uint, unlock-height: uint}))
    (+ amount (get amount commission))
)
```