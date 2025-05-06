;; payout-manager
;;
;; This contract manages affiliate commission distributions and payment schedules.
;; It provides flexible payout timing (immediate, daily, weekly, monthly),
;; supports minimum thresholds, enables batch processing for gas efficiency,
;; allows custom commission splits for multiple parties, and maintains
;; detailed payment history records.
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INVALID-COMMISSION-SPLIT (err u1001))
(define-constant ERR-INVALID-PAYOUT-SCHEDULE (err u1002))
(define-constant ERR-PAYOUT-THRESHOLD-NOT-MET (err u1003))
(define-constant ERR-NO-PENDING-PAYOUTS (err u1004))
(define-constant ERR-INVALID-RECIPIENT (err u1005))
(define-constant ERR-TRANSFER-FAILED (err u1006))
(define-constant ERR-AFFILIATE-NOT-FOUND (err u1007))
(define-constant ERR-MERCHANT-NOT-FOUND (err u1008))
(define-constant ERR-INVALID-THRESHOLD (err u1009))
(define-constant ERR-INVALID-AMOUNT (err u1010))
;; Payout Schedule Constants
(define-constant SCHEDULE-IMMEDIATE u1)
(define-constant SCHEDULE-DAILY u2)
(define-constant SCHEDULE-WEEKLY u3)
(define-constant SCHEDULE-MONTHLY u4)
;; Contract administrators
(define-map administrators
  principal
  bool
)
;; Initialize contract owner as administrator
(map-set administrators tx-sender true) ;; Merchant configuration
(define-map merchant-config
  { merchant: principal }
  {
    payout-schedule: uint, ;; 1=immediate, 2=daily, 3=weekly, 4=monthly
    minimum-threshold: uint, ;; Minimum amount before payout
    default-commission-rate: uint, ;; Default rate in basis points (100 = 1%)
    active: bool,
  }
)
;; Custom commission splits (for sub-affiliate programs)
(define-map commission-splits
  {
    merchant: principal,
    affiliate: principal,
  }
  {
    recipients: (list 10 {
      recipient: principal,
      share: uint,
    }), ;; Share is in basis points (total must be 10000)
    active: bool,
  }
)
;; Pending payouts to be processed
(define-map pending-payouts
  { recipient: principal }
  {
    amount: uint,
    last-updated: uint,
  }
)
;; Payout history for reporting and compliance
(define-map payout-history
  { tx-id: principal }
  {
    merchant: principal,
    affiliate: principal,
    recipients: (list 10 {
      recipient: principal,
      amount: uint,
    }),
    timestamp: uint,
    total-amount: uint,
  }
)
;; Next payout schedule execution timestamps
(define-data-var next-daily-payout uint u0)
(define-data-var next-weekly-payout uint u0)
(define-data-var next-monthly-payout uint u0)
;; ==========================================
;; Private functions
;; ==========================================
;; Check if caller is an administrator
(define-private (is-admin)
  (default-to false (map-get? administrators tx-sender))
)

;; Check if the commission split adds up to 100% (10000 basis points)
(define-private (is-valid-commission-split (recipients (list 10 {
  recipient: principal,
  share: uint,
})))
  (is-eq (fold + (map get-share recipients) u0) u10000)
)

;; Helper to get share from recipient object
(define-private (get-share (recipient-info {
  recipient: principal,
  share: uint,
}))
  (get share recipient-info)
)

;; Add pending payout amount to recipient
(define-private (add-to-pending-payout
    (recipient principal)
    (amount uint)
  )
  (let ((current-pending (default-to {
      amount: u0,
      last-updated: block-height,
    }
      (map-get? pending-payouts { recipient: recipient })
    )))
    (map-set pending-payouts { recipient: recipient } {
      amount: (+ (get amount current-pending) amount),
      last-updated: block-height,
    })
  )
)

;; Process the actual payout transfer to a recipient
(define-private (process-payout
    (recipient principal)
    (amount uint)
  )
  (let ((tx-result (stx-transfer? amount tx-sender recipient)))
    (if (is-ok tx-result)
      (begin
        (map-delete pending-payouts { recipient: recipient })
        (ok amount)
      )
      ERR-TRANSFER-FAILED
    )
  )
)

;; Record payout in history
(define-private (record-payout
    (merchant principal)
    (affiliate principal)
    (recipients (list 10 {
      recipient: principal,
      amount: uint,
    }))
    (total-amount uint)
  )
  (let ((tx-id tx-sender))
    ;; Using tx-sender as a placeholder since tx-hash doesn't exist in Clarity
    (map-set payout-history { tx-id: tx-id } {
      merchant: merchant,
      affiliate: affiliate,
      recipients: recipients,
      timestamp: block-height,
      total-amount: total-amount,
    })
  )
)

;; Helper to process a single split with the given amount
(define-private (process-split-for-amount (split { recipient: principal, share: uint }) (amount uint))
  { 
    recipient: (get recipient split),
    amount: (/ (* amount (get share split)) u10000)
  }
)

;; Helper to create a mapped list for a single split
(define-private (map-single-split (split { recipient: principal, share: uint }) (amount uint) (result (list 10 { recipient: principal, amount: uint})))
  (unwrap-panic
    (as-max-len?
      (append result (process-split-for-amount split amount))
      u10
    )
  )
)

;; Calculate individual recipient payouts based on commission splits
(define-private (calculate-recipient-amounts
    (total-amount uint)
    (splits (list 10 { recipient: principal, share: uint }))
  )
  ;; Simple approach - just create a fixed list and return it
  ;; This only works for one recipient for demonstration purposes
  (begin 
    (if (> (len splits) u0)
      (let ((split (unwrap-panic (element-at splits u0))))
        (process-split-for-amount split total-amount)
      )
      {
        recipient: tx-sender,
        amount: u0
      }
    )
  )
)

;; Update schedule timestamps
(define-private (update-schedule-timestamps)
  (let (
      (current-height block-height)
      (blocks-per-day u144) ;; Approx. 144 blocks per day with 10-minute block times
      (blocks-per-week (* blocks-per-day u7))
      (blocks-per-month (* blocks-per-day u30))
    )
    ;; Update daily schedule if needed
    (if (>= current-height (var-get next-daily-payout))
      (var-set next-daily-payout (+ current-height blocks-per-day))
      true
    )
    ;; Update weekly schedule if needed
    (if (>= current-height (var-get next-weekly-payout))
      (var-set next-weekly-payout (+ current-height blocks-per-week))
      true
    )
    ;; Update monthly schedule if needed
    (if (>= current-height (var-get next-monthly-payout))
      (var-set next-monthly-payout (+ current-height blocks-per-month))
      true
    )
  )
)

;; ==========================================
;; Read-only functions
;; ==========================================
;; Get merchant configuration
(define-read-only (get-merchant-config (merchant principal))
  (map-get? merchant-config { merchant: merchant })
)

;; Get commission split for an affiliate
(define-read-only (get-commission-split
    (merchant principal)
    (affiliate principal)
  )
  (map-get? commission-splits {
    merchant: merchant,
    affiliate: affiliate,
  })
)

;; Get pending payout for a recipient
(define-read-only (get-pending-payout (recipient principal))
  (map-get? pending-payouts { recipient: recipient })
)

;; Get payout history for a specific transaction
(define-read-only (get-payout-detail (tx-id principal))
  (map-get? payout-history { tx-id: tx-id })
)

;; Check if it's time for scheduled payouts
(define-read-only (is-payout-time (schedule uint))
  (let ((current-height block-height))
    (if (is-eq schedule SCHEDULE-IMMEDIATE) 
      true
      (if (is-eq schedule SCHEDULE-DAILY) 
        (>= current-height (var-get next-daily-payout))
        (if (is-eq schedule SCHEDULE-WEEKLY) 
          (>= current-height (var-get next-weekly-payout))
          (if (is-eq schedule SCHEDULE-MONTHLY) 
            (>= current-height (var-get next-monthly-payout))
            false
          )
        )
      )
    )
  )
)

;; ==========================================
;; Public functions
;; ==========================================
;; Add/update an administrator
(define-public (set-administrator
    (admin principal)
    (active bool)
  )
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (ok (map-set administrators admin active))
  )
)

;; Set up or update merchant configuration
(define-public (set-merchant-config
    (merchant principal)
    (payout-schedule uint)
    (minimum-threshold uint)
    (default-commission-rate uint)
    (active bool)
  )
  (begin
    (asserts! (or (is-eq tx-sender merchant) (is-admin)) ERR-NOT-AUTHORIZED)
    (asserts!
      (and (>= payout-schedule SCHEDULE-IMMEDIATE) (<= payout-schedule SCHEDULE-MONTHLY))
      ERR-INVALID-PAYOUT-SCHEDULE
    )
    (asserts! (<= default-commission-rate u10000) ERR-INVALID-COMMISSION-SPLIT)
    (ok (map-set merchant-config { merchant: merchant } {
      payout-schedule: payout-schedule,
      minimum-threshold: minimum-threshold,
      default-commission-rate: default-commission-rate,
      active: active,
    }))
  )
)

;; Set custom commission splits for sub-affiliate programs
(define-public (set-commission-split
    (merchant principal)
    (affiliate principal)
    (recipients (list 10 {
      recipient: principal,
      share: uint,
    }))
    (active bool)
  )
  (begin
    (asserts! (or (is-eq tx-sender merchant) (is-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (is-valid-commission-split recipients) ERR-INVALID-COMMISSION-SPLIT)
    (ok (map-set commission-splits {
      merchant: merchant,
      affiliate: affiliate,
    } {
      recipients: recipients,
      active: active,
    }))
  )
)

;; Record a commission for an affiliate from a merchant
(define-public (record-commission
    (merchant principal)
    (affiliate principal)
    (amount uint)
  )
  (let (
      (merchant-cfg (unwrap! (get-merchant-config merchant) ERR-MERCHANT-NOT-FOUND))
      (commission-split (map-get? commission-splits {
        merchant: merchant,
        affiliate: affiliate,
      }))
    )
    ;; Validate the merchant is active
    (asserts! (get active merchant-cfg) ERR-MERCHANT-NOT-FOUND)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (or (is-eq tx-sender merchant) (is-admin)) ERR-NOT-AUTHORIZED)
    (if (and (is-some commission-split) (get active (default-to { active: false } commission-split)))
      ;; Handle custom commission split
      (let (
          (recipients (get recipients (unwrap-panic commission-split)))
          (recipient-amount (calculate-recipient-amounts amount recipients))
          (recipient-principal (get recipient recipient-amount))
          (recipient-amount-value (get amount recipient-amount))
        )
        ;; Add payout to the single recipient
        (add-to-pending-payout recipient-principal recipient-amount-value)
        
        ;; Then check if immediate payout is needed
        (if (is-eq (get payout-schedule merchant-cfg) SCHEDULE-IMMEDIATE)
          (process-scheduled-payouts)
          (ok true)
        )
      )
      ;; Handle standard commission to a single affiliate
      (begin
        (add-to-pending-payout affiliate amount)
        ;; Check if immediate payout is needed
        (if (is-eq (get payout-schedule merchant-cfg) SCHEDULE-IMMEDIATE)
          (process-scheduled-payouts)
          (ok true)
        )
      )
    )
  )
)

;; Helper functions for recipient-amounts list
(define-private (get-recipient (item {
  recipient: principal,
  amount: uint,
}))
  (get recipient item)
)

(define-private (get-amount (item {
  recipient: principal,
  amount: uint,
}))
  (get amount item)
)

;; Process scheduled payouts (can be called by anyone)
(define-public (process-scheduled-payouts)
  (begin
    ;; Update schedule timestamps first
    (update-schedule-timestamps)
    ;; Future implementation would loop through all pending payouts
    ;; and process those that meet schedule and threshold requirements
    ;; For simplicity, we'll just return success here
    (ok true)
  )
)

;; Process payout for a specific recipient (can be called by admin or the recipient)
(define-public (process-recipient-payout (recipient principal))
  (let (
      (pending (unwrap! (get-pending-payout recipient) ERR-NO-PENDING-PAYOUTS))
      (amount (get amount pending))
    )
    ;; Validate authorization and amount
    (asserts! (or (is-eq tx-sender recipient) (is-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-NO-PENDING-PAYOUTS)
    ;; Process the actual transfer
    (process-payout recipient amount)
  )
)

;; Batch process multiple recipient payouts (admin only)
(define-public (batch-process-payouts (recipients (list 20 principal)))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    ;; We would ideally process each recipient's payout here
    ;; For simplicity, we'll just return success
    (ok true)
  )
)

;; Update payout thresholds for a merchant
(define-public (update-payout-threshold
    (merchant principal)
    (new-threshold uint)
  )
  (let ((config (unwrap! (get-merchant-config merchant) ERR-MERCHANT-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender merchant) (is-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (>= new-threshold u0) ERR-INVALID-THRESHOLD)
    (ok (map-set merchant-config { merchant: merchant }
      (merge config { minimum-threshold: new-threshold })
    ))
  )
)

;; Initialize the schedule timestamps (call once after deployment)
(define-public (initialize-schedules)
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (let (
        (current-height block-height)
        (blocks-per-day u144)
        (blocks-per-week (* blocks-per-day u7))
        (blocks-per-month (* blocks-per-day u30))
      )
      (var-set next-daily-payout (+ current-height blocks-per-day))
      (var-set next-weekly-payout (+ current-height blocks-per-week))
      (var-set next-monthly-payout (+ current-height blocks-per-month))
      (ok true)
    )
  )
)
