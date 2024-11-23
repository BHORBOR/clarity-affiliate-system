;; Affiliate Marketing Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-not-found (err u102))
(define-constant err-insufficient-funds (err u103))

;; Data vars
(define-data-var commission-rate uint u10) ;; 10% default commission

;; Data maps
(define-map affiliates principal
    {
        earned: uint,
        referrals: uint,
        active: bool
    }
)

(define-map products uint
    {
        price: uint,
        owner: principal,
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
                    active: true
                })
                (ok true)
            )
        )
    )
)

(define-public (add-product (product-id uint) (price uint))
    (begin
        (map-set products product-id {
            price: price,
            owner: tx-sender,
            active: true
        })
        (ok true)
    )
)

(define-public (process-sale (product-id uint) (affiliate principal))
    (let (
        (product (unwrap! (map-get? products product-id) err-not-found))
        (affiliate-data (unwrap! (map-get? affiliates affiliate) err-not-found))
        (commission (/ (* (get price product) (var-get commission-rate)) u100))
    )
    (if (>= (stx-get-balance tx-sender) (get price product))
        (begin
            ;; Transfer payment to product owner
            (try! (stx-transfer? (- (get price product) commission) tx-sender (get owner product)))
            ;; Transfer commission to affiliate
            (try! (stx-transfer? commission tx-sender affiliate))
            ;; Update affiliate stats
            (map-set affiliates affiliate {
                earned: (+ (get earned affiliate-data) commission),
                referrals: (+ (get referrals affiliate-data) u1),
                active: true
            })
            (ok true)
        )
        err-insufficient-funds
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
